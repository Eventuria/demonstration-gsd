module Gsd.CommandPredicates where

import Cqrs.Aggregate.Snapshots.AggregateSnapshot
import Cqrs.Streams

isAlreadyProcessed :: Offset -> Maybe AggregateSnapshot -> Bool
isAlreadyProcessed offset snapshotMaybe = Just offset == (lastOffsetConsumed <$> snapshotMaybe)

isFirstCommand :: Maybe AggregateSnapshot -> Bool
isFirstCommand snapshotMaybe = snapshotMaybe == Nothing


