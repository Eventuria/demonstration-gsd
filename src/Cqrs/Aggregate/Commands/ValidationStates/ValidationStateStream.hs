{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
module Cqrs.Aggregate.Commands.ValidationStates.ValidationStateStream where

import Cqrs.Aggregate.Commands.ValidationStates.ValidationState
import qualified Database.EventStore as EventStore
import qualified Data.Text as Text
import Data.UUID

import Cqrs.Aggregate.Commands.ValidationStates.PersistedValidationState
import Data.Maybe

import Cqrs.EventStore.Querying
import Cqrs.Streams
import Cqrs.EventStore.Stream
import Cqrs.EventStore.Context
import Cqrs.Aggregate.Ids.AggregateId
import qualified Cqrs.EventStore.Writing as EventStore.Writing

type ValidateStateStream = EventStoreStream PersistedValidationState


persist :: EventStoreContext -> ValidationState -> IO (Either PersistenceFailure PersistResult)
persist context validationState @ ValidationState { state = AggregateState { aggregateId = aggregateId}} = EventStore.Writing.persist context (getStreamName aggregateId) validationState

retrieveLastOffsetConsumed :: ValidateStateStream -> IO (Maybe Offset)
retrieveLastOffsetConsumed validationStateStream = (fmap.fmap) ( \persistedValidationState -> lastOffsetConsumed $ validationState persistedValidationState ) (retrieveLast validationStateStream)


getValidateStateStream :: EventStoreContext -> AggregateId -> ValidateStateStream
getValidateStateStream context aggregateId = EventStoreStream {
                                                        context = context,
                                                        streamName = (getStreamName aggregateId),
                                                        recordedEventToPersistedItem = recordedEventToPersistedValidationState }

recordedEventToPersistedValidationState :: EventStore.RecordedEvent -> PersistedValidationState
recordedEventToPersistedValidationState recordedEvent =
  PersistedValidationState { offset = toInteger $ EventStore.recordedEventNumber recordedEvent,
                               validationState = fromJust $ EventStore.recordedEventDataAsJson recordedEvent }


getStreamName :: AggregateId -> EventStore.StreamName
getStreamName aggregateId = EventStore.StreamName $ Text.pack $ "aggregate_validation_state-" ++ (toString aggregateId)

instance EventStore.Writing.Persistable ValidationState where
  getItemName validationState  = "validationState"

