module Gsd.CommandPredicates where

import Cqrs.Aggregate.Commands.ValidationStates.ValidationState
import Cqrs.Streams

isAlreadyProcessed :: Offset -> Maybe ValidationState -> Bool
isAlreadyProcessed offset snapshotMaybe = Just offset <= (lastOffsetConsumed <$> snapshotMaybe)

isFirstCommand :: Maybe ValidationState -> Bool
isFirstCommand snapshotMaybe = snapshotMaybe == Nothing


