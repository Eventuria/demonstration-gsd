{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Cqrs.Aggregate.Ids.AggregateIdStream  where

import Cqrs.Aggregate.Commands.CommandStream
import Prelude hiding (catch)


import Cqrs.Aggregate.Ids.AggregateId
import Cqrs.Aggregate.Commands.Command
import qualified Database.EventStore as EventStore
import qualified Data.Text as Text
import Data.UUID
import Data.ByteString.Char8 as Char8 (unpack)


import Streamly
import qualified Streamly.Prelude as S
import Control.Monad.IO.Class (MonadIO)
import Data.Function ((&))
import Cqrs.EventStore.Stream
import Cqrs.EventStore.Subscribing

import Cqrs.EventStore.Context
import Cqrs.EventStore.PersistedItem

type AggregateIdStream = EventStoreStream AggregateId

type CorruptedStreamName = String

getAggregateStream :: EventStoreContext -> AggregateIdStream
getAggregateStream eventStoreContext = EventStoreStream { context = eventStoreContext,
                                                          streamName = getAggregateCreatedStreamName,
                                                          recordedEventToPersistedItem = recordedEventToPersistedAggregate }

getAggregateCreatedStreamName :: EventStore.StreamName
getAggregateCreatedStreamName = EventStore.StreamName $ Text.pack $ "$et-createWorkspace"


yieldAndSubscribeToAggregateUpdates :: (IsStream stream,
                                        MonadIO (stream IO),
                                        Semigroup (stream IO (Persisted AggregateId)),
                                        Semigroup (stream IO (Persisted Command)))  =>
                                       EventStoreContext ->
                                       Persisted AggregateId ->
                                       stream IO (Persisted AggregateId)
yieldAndSubscribeToAggregateUpdates eventStoreContext persistedAggregate @ PersistedItem { offset = offset , item = aggregateId} =
  (S.yield persistedAggregate) <>
  ((subscribeToAggregateUpdates eventStoreContext aggregateId  ) & S.map (\aggregateId -> PersistedItem { offset = offset,item = aggregateId }))


subscribeToAggregateUpdates :: (IsStream stream,
                                MonadIO (stream IO),
                                Semigroup (stream IO (Persisted Command)))  =>
                                  EventStoreContext ->
                                  AggregateId ->
                                  stream IO AggregateId
subscribeToAggregateUpdates eventStoreContext aggregateId =
  (subscribe $ getCommandStream eventStoreContext aggregateId) & S.map (\persistedCommand -> aggregateId)


recordedEventToPersistedAggregate :: EventStore.RecordedEvent -> Persisted AggregateId
recordedEventToPersistedAggregate recordedEvent =
  PersistedItem { offset = toInteger $ EventStore.recordedEventNumber recordedEvent,
                  item = case (fromString $ drop (length  ("0@aggregate_command-" :: [Char])) $ unpack $ EventStore.recordedEventData recordedEvent) of
                                          Just aggregateId -> aggregateId
                                          Nothing -> error $ "Aggregate stream corrupted :" ++ (unpack $ EventStore.recordedEventData recordedEvent) }

