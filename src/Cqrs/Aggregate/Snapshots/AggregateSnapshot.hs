module Cqrs.Aggregate.Snapshots.AggregateSnapshot where

import Cqrs.Streams
import Data.Set (Set)
import Cqrs.Commands.CommandId
import Cqrs.Aggregate.Ids.AggregateId

data AggregateSnapshot = AggregateSnapshot { lastOffsetConsumed::Offset , commandsProcessed :: Set CommandId, state :: AggregateState } deriving (Eq)

instance Show AggregateSnapshot where
  show snapshot = "Snapshot { offset = " ++ ( show $ lastOffsetConsumed snapshot)  ++ " }"

data AggregateState = AggregateState {aggregateId :: AggregateId } deriving (Eq,Show)
