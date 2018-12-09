
module Cqrs.Aggregate.Commands.ValidationStates.ValidationState where

import Data.Set (Set)

import Cqrs.PersistedStreamEngine.Offset
import Cqrs.Aggregate.Commands.CommandId
import Cqrs.Aggregate.Ids.AggregateId
import Cqrs.Aggregate.Core

data ValidationState = ValidationState { lastOffsetConsumed::Offset , commandsProcessed :: Set CommandId, state :: AggregateState } deriving (Eq)

instance AggregateJoinable ValidationState where
  getAggregateId ValidationState { state = AggregateState {aggregateId = aggregateId} } = aggregateId

instance Show ValidationState where
  show validationState = "ValidationState { offset = " ++ ( show $ lastOffsetConsumed validationState)  ++ " }"

data AggregateState = AggregateState {aggregateId :: AggregateId } deriving (Eq,Show)



