
module Cqrs.Write.Aggregate.Commands.ValidationStates.ValidationState where

import Data.Set (Set)

import PersistedStreamEngine.Offset
import Cqrs.Write.Aggregate.Commands.CommandId
import Cqrs.Write.Aggregate.Ids.AggregateId
import Cqrs.Write.Aggregate.Core

data ValidationState = ValidationState { lastOffsetConsumed::Offset , commandsProcessed :: Set CommandId, state :: AggregateState } deriving (Eq)

instance AggregateJoinable ValidationState where
  getAggregateId ValidationState { state = AggregateState {aggregateId = aggregateId} } = aggregateId

instance Show ValidationState where
  show validationState = "ValidationState { offset = " ++ ( show $ lastOffsetConsumed validationState)  ++ " }"

data AggregateState = AggregateState {aggregateId :: AggregateId } deriving (Eq,Show)



