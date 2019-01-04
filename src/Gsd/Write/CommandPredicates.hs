module Gsd.Write.CommandPredicates where

import Cqrs.Write.Aggregate.Commands.ValidationStates.ValidationState
import PersistedStreamEngine.Interface.Offset

isAlreadyProcessed :: Offset -> Maybe ValidationState -> Bool
isAlreadyProcessed offset snapshotMaybe = Just offset <= (lastOffsetConsumed <$> snapshotMaybe)

isFirstCommand :: Maybe ValidationState -> Bool
isFirstCommand snapshotMaybe = snapshotMaybe == Nothing

isNotFirstCommand :: Maybe ValidationState -> Bool
isNotFirstCommand = not . isFirstCommand
