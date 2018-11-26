{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Cqrs.EventStore.Streaming where

import Streamly
import qualified Streamly.Prelude as S
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent.Async (wait)

import qualified Database.EventStore as EventStore
import qualified Cqrs.EventStore.Subscribing as EventStore.Subscribing
import Cqrs.EventStore.PersistedItem
import Cqrs.Logger
import Cqrs.Streams (Offset)
import Cqrs.EventStore.Stream
import Cqrs.EventStore.Context
import Data.Aeson
import Data.Maybe

isStreamNotExistRequest :: EventStoreStream item -> IO Bool
isStreamNotExistRequest EventStoreStream { context = Context { logger = logger,
                                                     credentials = credentials,
                                                     connection = connection },
                                 streamName = streamName} = do
   let resolveLinkTos = False
   asyncRead <- EventStore.readStreamEventsForward
                    connection
                    streamName
                    (fromInteger 0)
                    (fromInteger 1)
                    resolveLinkTos
                    (Just credentials)
   commandFetched <- liftIO $ wait asyncRead
   return $ case commandFetched of
        EventStore.ReadNoStream -> True
        _ -> False

streamAllInfinitely ::  (FromJSON item,
                         IsStream stream,
                         MonadIO (stream IO),
                         Semigroup (stream IO (Persisted item))) =>
                          EventStoreStream item ->
                          stream IO (Persisted item)
streamAllInfinitely eventStoreStream =
  (EventStore.Subscribing.subscribe eventStoreStream)
    `parallel` (streamAll eventStoreStream)

streamAll :: (FromJSON item,
              IsStream stream,
              MonadIO (stream IO),
              Semigroup (stream IO (Persisted item))) =>
                EventStoreStream item ->
                stream IO (Persisted item)
streamAll eventStoreStream = streamFromOffset eventStoreStream 0

streamFromOffset :: (FromJSON item,
                     IsStream stream,
                     MonadIO (stream IO),
                     Semigroup (stream IO (Persisted item))) =>
                      EventStoreStream item ->
                      Offset ->
                      stream IO (Persisted item)


streamFromOffset eventStoreStream @ EventStoreStream {
                                       context = Context { logger = logger,
                                                           credentials = credentials,
                                                           connection = connection },
                                       streamName = streamName } fromOffset = do
     liftIO $ logInfo logger $ "starting streaming from offset " ++ (show fromOffset)
     let batchSize = 100 :: Integer
         resolveLinkTos = False
     asyncRead <- liftIO $ EventStore.readStreamEventsForward
                      connection
                      streamName
                      (fromInteger fromOffset)
                      (fromInteger batchSize)
                      resolveLinkTos
                      (Just credentials)
     commandFetched <- liftIO $ wait asyncRead
     case commandFetched of
          EventStore.ReadSuccess readResult -> do
              let commandStreamNamesByAggregateId = recordedEventToPersistedItem
                                                    <$> EventStore.resolvedEventOriginal
                                                    <$> EventStore.sliceEvents readResult
              if (length commandStreamNamesByAggregateId) /= 0 then do
                  (S.fromList commandStreamNamesByAggregateId) <>
                    (streamFromOffset eventStoreStream $ fromOffset + batchSize)
              else S.fromList commandStreamNamesByAggregateId
          EventStore.ReadNoStream -> S.fromList []
          e -> error $ "Read failure: " <> show e


recordedEventToPersistedItem :: FromJSON item => EventStore.RecordedEvent -> Persisted item
recordedEventToPersistedItem recordedEvent =
  PersistedItem { offset = toInteger $ EventStore.recordedEventNumber recordedEvent,
                  item = fromJust $ EventStore.recordedEventDataAsJson recordedEvent }