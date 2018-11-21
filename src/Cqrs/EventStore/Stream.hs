{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Cqrs.EventStore.Stream where

import Streamly
import qualified Streamly.Prelude as S
import Control.Concurrent
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Function ((&))
import Control.Concurrent.Async (wait)

import qualified Database.EventStore as EventStore

import Cqrs.Logger
import Cqrs.PersistedAggregate
import Cqrs.Streams


streamAll :: (IsStream stream,
              MonadIO (stream IO),
              Semigroup (stream IO persistedItem)) =>
                Logger ->
                EventStore.Credentials ->
                EventStore.StreamName ->
                EventStore.Connection ->
                (EventStore.RecordedEvent -> persistedItem) ->
                stream IO persistedItem
streamAll logger credentials streamName eventStoreConnection recordedEventToPersistedItem = streamFromOffset logger credentials streamName eventStoreConnection recordedEventToPersistedItem 0

streamFromOffset :: (IsStream stream,
                     MonadIO (stream IO),
                     Semigroup (stream IO persistedItem)) =>
                      Logger ->
                      EventStore.Credentials ->
                      EventStore.StreamName ->
                      EventStore.Connection ->
                      (EventStore.RecordedEvent -> persistedItem) ->
                      Offset ->
                      stream IO persistedItem
streamFromOffset logger credentials streamName eventStoreConnection recordedEventToPersistedItem fromOffset = do
     liftIO $ logInfo logger $ "starting streaming from offset " ++ (show fromOffset)
     let batchSize = 100 :: Integer
         resolveLinkTos = False
     asyncRead <- liftIO $ EventStore.readStreamEventsForward
                      eventStoreConnection
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
                    (streamFromOffset logger credentials streamName eventStoreConnection recordedEventToPersistedItem $ fromOffset + batchSize)
              else S.fromList commandStreamNamesByAggregateId
          EventStore.ReadNoStream -> error $ "ReadNoStream "
          e -> error $ "Read failure: " <> show e

