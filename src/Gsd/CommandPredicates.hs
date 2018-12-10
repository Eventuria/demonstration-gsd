module Gsd.CommandPredicates where

import Cqrs.Write.Aggregate.Commands.ValidationStates.ValidationState
import PersistedStreamEngine.Offset

isAlreadyProcessed :: Offset -> Maybe ValidationState -> Bool
isAlreadyProcessed offset snapshotMaybe = Just offset <= (lastOffsetConsumed <$> snapshotMaybe)

isFirstCommand :: Maybe ValidationState -> Bool
isFirstCommand snapshotMaybe = snapshotMaybe == Nothing


