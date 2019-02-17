module Gsd.Write.CommandPredicates where

import CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import PersistedStreamEngine.Interface.Offset
import Gsd.Write.Model.State


isAlreadyProcessed :: Offset -> Maybe (ValidationState GsdState) -> Bool
isAlreadyProcessed offset snapshotMaybe = Just offset <= (lastOffsetConsumed <$> snapshotMaybe)

isFirstCommand :: Offset -> Bool
isFirstCommand 0 = True
isFirstCommand _ = False

isNotFirstCommand :: Offset -> Bool
isNotFirstCommand = not . isFirstCommand
