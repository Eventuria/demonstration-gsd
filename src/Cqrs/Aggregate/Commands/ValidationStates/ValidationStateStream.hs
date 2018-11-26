{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
module Cqrs.Aggregate.Commands.ValidationStates.ValidationStateStream where

import Cqrs.Aggregate.Commands.ValidationStates.ValidationState
import qualified Database.EventStore as EventStore
import qualified Data.Text as Text
import Data.UUID

import Cqrs.EventStore.Querying
import Cqrs.Streams
import Cqrs.EventStore.Stream
import Cqrs.EventStore.Context
import Cqrs.Aggregate.Ids.AggregateId
import qualified Cqrs.EventStore.Writing as EventStore.Writing
import Cqrs.EventStore.PersistedItem

type ValidateStateStream = EventStoreStream ValidationState


retrieveLastOffsetConsumed :: ValidateStateStream -> IO (Maybe Offset)
retrieveLastOffsetConsumed validationStateStream = (fmap.fmap) ( \persistedValidationState -> lastOffsetConsumed $ item $ persistedValidationState ) (retrieveLast validationStateStream)


getValidateStateStream :: EventStoreContext -> AggregateId -> ValidateStateStream
getValidateStateStream context aggregateId = EventStoreStream {
                                                        context = context,
                                                        streamName = (getStreamName aggregateId) }

getStreamName :: AggregateId -> EventStore.StreamName
getStreamName aggregateId = EventStore.StreamName $ Text.pack $ "aggregate_validation_state-" ++ (toString aggregateId)

instance EventStore.Writing.Writable ValidationState where
  getItemName validationState  = "validationState"


