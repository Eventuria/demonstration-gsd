module Gsd.CommandPredicates where

import Cqrs.Snapshot
import Cqrs.Command
import Cqrs.Streams
import Gsd.Commands

isAlreadyProcessed :: Offset -> Maybe AggregateSnapshot -> Bool
isAlreadyProcessed offset snapshotMaybe = Just offset == (lastOffsetConsumed <$> snapshotMaybe)

isFirstCommand :: Maybe AggregateSnapshot -> Bool
isFirstCommand snapshotMaybe = snapshotMaybe == Nothing


