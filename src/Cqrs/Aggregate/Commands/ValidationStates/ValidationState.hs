module Cqrs.Aggregate.Commands.ValidationStates.ValidationState where

import Cqrs.Streams
import Data.Set (Set)
import Cqrs.Aggregate.Commands.CommandId
import Cqrs.Aggregate.Ids.AggregateId

data ValidationState = ValidationState { lastOffsetConsumed::Offset , commandsProcessed :: Set CommandId, state :: AggregateState } deriving (Eq)

instance Show ValidationState where
  show validationState = "ValidationState { offset = " ++ ( show $ lastOffsetConsumed validationState)  ++ " }"

data AggregateState = AggregateState {aggregateId :: AggregateId } deriving (Eq,Show)
