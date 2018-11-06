module Cqrs.Snapshot where
import Cqrs.Core
import Cqrs.Streams
import Data.Set (Set)
import qualified Data.Set as Set


data AggregateSnapshot = AggregateSnapshot { lastOffsetConsumed::Offset , commandsProcessed :: Set CommandId, state :: AggregateState } deriving (Eq)

instance Show AggregateSnapshot where
  show snapshot = "Snapshot { offset = " ++ ( show $ lastOffsetConsumed snapshot)  ++ " }"

data AggregateState = AggregateState {aggregateId :: AggregateId } deriving (Eq,Show)
