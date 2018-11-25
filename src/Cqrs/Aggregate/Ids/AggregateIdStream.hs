{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Cqrs.Aggregate.Ids.AggregateIdStream  where

import Cqrs.Aggregate.Commands.CommandStream
import Prelude hiding (catch)


import Cqrs.Aggregate.Ids.AggregateId
import qualified Database.EventStore as EventStore
import qualified Data.Text as Text
import Data.UUID
import Data.ByteString.Char8 as Char8 (unpack)
import Cqrs.Aggregate.Ids.PersistedAggregateId

import Streamly
import qualified Streamly.Prelude as S
import Control.Monad.IO.Class (MonadIO)
import Data.Function ((&))
import Cqrs.EventStore.Stream
import Cqrs.EventStore.Subscribing
import Cqrs.Aggregate.Commands.PersistedCommand
import Cqrs.EventStore.Context

type AggregateStream = EventStoreStream PersistedAggregateId

type CorruptedStreamName = String

getAggregateStream :: EventStoreContext -> AggregateStream
getAggregateStream eventStoreContext = EventStoreStream { context = eventStoreContext,
                                                          streamName = getAggregateCreatedStreamName,
                                                          recordedEventToPersistedItem = recordedEventToPersistedAggregate }

getAggregateCreatedStreamName :: EventStore.StreamName
getAggregateCreatedStreamName = EventStore.StreamName $ Text.pack $ "$et-createWorkspace"


yieldAndSubscribeToAggregateUpdates :: (IsStream stream,
                                        MonadIO (stream IO),
                                        Semigroup (stream IO PersistedAggregateId),
                                        Semigroup (stream IO (PersistedCommand)))  =>
                                       EventStoreContext ->
                                       PersistedAggregateId ->
                                       stream IO PersistedAggregateId
yieldAndSubscribeToAggregateUpdates eventStoreContext persistedAggregate @ PersistedAggregateId { offset = offset , persistedAggregateId = aggregateId} =
  (S.yield persistedAggregate) <>
  ((subscribeToAggregateUpdates eventStoreContext aggregateId  ) & S.map (\aggregateId -> PersistedAggregateId { offset = offset,persistedAggregateId = aggregateId }))


subscribeToAggregateUpdates :: (IsStream stream,
                                MonadIO (stream IO),
                                Semigroup (stream IO (PersistedCommand)))  =>
                                  EventStoreContext ->
                                  AggregateId ->
                                  stream IO AggregateId
subscribeToAggregateUpdates eventStoreContext aggregateId =
  (subscribe $ getCommandStream eventStoreContext aggregateId) & S.map (\persistedCommand -> aggregateId)


recordedEventToPersistedAggregate :: EventStore.RecordedEvent -> PersistedAggregateId
recordedEventToPersistedAggregate recordedEvent =
  PersistedAggregateId { offset = toInteger $ EventStore.recordedEventNumber recordedEvent,
                       persistedAggregateId = case (fromString $ drop (length  ("0@aggregate_command-" :: [Char])) $ unpack $ EventStore.recordedEventData recordedEvent) of
                                          Just aggregateId -> aggregateId
                                          Nothing -> error $ "Aggregate stream corrupted :" ++ (unpack $ EventStore.recordedEventData recordedEvent) }

