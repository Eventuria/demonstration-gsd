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

import Cqrs.Logger
import Cqrs.Streams (Offset)
import Cqrs.EventStore.Stream
import Cqrs.EventStore.Context

streamAllInfinitely ::  (IsStream stream,
                         MonadIO (stream IO),
                         Semigroup (stream IO persistedItem)) =>
                          EventStoreStream persistedItem ->
                          stream IO persistedItem
streamAllInfinitely eventStoreStream =
  (EventStore.Subscribing.subscribe eventStoreStream)
    `parallel` (streamAll eventStoreStream)

streamAll :: (IsStream stream,
              MonadIO (stream IO),
              Semigroup (stream IO persistedItem)) =>
                EventStoreStream persistedItem ->
                stream IO persistedItem
streamAll eventStoreStream = streamFromOffset eventStoreStream 0

streamFromOffset :: (IsStream stream,
                     MonadIO (stream IO),
                     Semigroup (stream IO persistedItem)) =>
                      EventStoreStream persistedItem ->
                      Offset ->
                      stream IO persistedItem
streamFromOffset eventStoreStream @ EventStoreStream {
                                       context = Context { logger = logger,
                                                           credentials = credentials,
                                                           connection = connection },
                                       streamName = streamName,
                                       recordedEventToPersistedItem = recordedEventToPersistedItem } fromOffset = do
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
          EventStore.ReadNoStream -> error $ "ReadNoStream "
          e -> error $ "Read failure: " <> show e


