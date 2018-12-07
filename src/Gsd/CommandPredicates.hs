module Gsd.CommandPredicates where

import Cqrs.Aggregate.Commands.ValidationStates.ValidationState
import Cqrs.PersistedStream.Offset

isAlreadyProcessed :: Offset -> Maybe ValidationState -> Bool
isAlreadyProcessed offset snapshotMaybe = Just offset <= (lastOffsetConsumed <$> snapshotMaybe)

isFirstCommand :: Maybe ValidationState -> Bool
isFirstCommand snapshotMaybe = snapshotMaybe == Nothing


