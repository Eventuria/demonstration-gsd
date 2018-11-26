{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Cqrs.Aggregate.Events.EventStream  where

import qualified Database.EventStore as EventStore
import Cqrs.Aggregate.Events.Event

import Cqrs.Aggregate.Ids.AggregateId
import qualified Data.Text as Text
import Data.UUID
import Data.Maybe
import qualified Cqrs.EventStore.Writing as EventStore.Writing
import Cqrs.EventStore.Context
import Cqrs.EventStore.Stream
import Cqrs.EventStore.PersistedItem

type EvenStream = EventStoreStream Event

getEventStream :: EventStoreContext -> AggregateId -> EvenStream
getEventStream context aggregateId = EventStoreStream {
                                                        context = context,
                                                        streamName = (getStreamName aggregateId),
                                                        recordedEventToPersistedItem = recordedEventToPersistedEvent }

recordedEventToPersistedEvent :: EventStore.RecordedEvent -> Persisted Event
recordedEventToPersistedEvent recordedEvent =
  PersistedItem { offset = toInteger $ EventStore.recordedEventNumber recordedEvent,
                    item = fromJust $ EventStore.recordedEventDataAsJson recordedEvent }

getStreamName :: AggregateId -> EventStore.StreamName
getStreamName workspaceId = EventStore.StreamName $ Text.pack $ "aggregate_event-" ++ toString workspaceId


instance EventStore.Writing.Writable Event where
  getItemName Event { eventHeader = EventHeader { eventName = eventName}} = eventName
