{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Gsd.Write.Commands.Handling.SetGoal where


import Cqrs.EDsl
import Gsd.Write.Events.Event
import Gsd.Write.State
import Cqrs.Write.Aggregate.Commands.ValidationStates.ValidationState
import Gsd.Write.Core
import Data.Text
import Cqrs.Write.Aggregate.Commands.CommandId
import PersistedStreamEngine.Interface.Offset
import Data.Set (fromList)

handle :: Offset -> CommandId -> WorkspaceId -> GoalId -> Text  -> CommandDirective GsdState
handle offset commandId workspaceId goalId goalDescription =
  Validate $ do
      now <- getCurrentTime
      eventId <- getNewEventID
      persistEvent $ toEvent $ GoalSet {  eventId , createdOn = now, workspaceId , goalId , goalDescription}
      updateValidationState ValidationState {lastOffsetConsumed = offset ,
                                             commandsProcessed = fromList [commandId] ,
                                             aggregateId = workspaceId,
                                             state = Just $ GsdState {goals = [Goal{workspaceId,goalId,description = goalDescription}]}}