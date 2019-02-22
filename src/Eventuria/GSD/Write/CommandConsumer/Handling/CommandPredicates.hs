module Eventuria.GSD.Write.CommandConsumer.Handling.CommandPredicates where

import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import Eventuria.GSD.Write.Model.State


isAlreadyProcessed :: Offset -> Maybe (ValidationState GsdState) -> Bool
isAlreadyProcessed offset snapshotMaybe = Just offset <= (lastOffsetConsumed <$> snapshotMaybe)

isFirstCommand :: Offset -> Bool
isFirstCommand 0 = True
isFirstCommand _ = False

isNotFirstCommand :: Offset -> Bool
isNotFirstCommand = not . isFirstCommand
