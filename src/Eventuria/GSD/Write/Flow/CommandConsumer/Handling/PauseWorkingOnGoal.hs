{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Eventuria.GSD.Write.Flow.CommandConsumer.Handling.PauseWorkingOnGoal where


import Eventuria.Libraries.CQRS.EDsl
import Eventuria.GSD.Write.Model.Events.Event
import Eventuria.GSD.Write.Model.State
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import Eventuria.GSD.Write.Model.Core

import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import Data.Set hiding (map)

import Data.List (find)

handle :: Offset -> ValidationState GsdState -> CommandId -> WorkspaceId -> GoalId ->  CommandDirective GsdState
handle offset ValidationState {commandsProcessed, aggregateId, state} commandId workspaceId goalId = case state of
  Nothing -> Reject "Trying to pause a goal but there are no goals in that workspace"
  Just GsdState {goals} -> case (findGoal goalId goals)  of
    Nothing -> Reject "Trying to pause a goal that does not exist"
    Just goal @ Goal {workspaceId,goalId,description,status} -> case status of
      InProgress ->
        Validate $ do
                now <- getCurrentTime
                eventId <- getNewEventID
                persistEvent $ toEvent $ GoalPaused {  eventId , createdOn = now, workspaceId , goalId}
                updateValidationState ValidationState {lastOffsetConsumed = offset ,
                                                       commandsProcessed = union commandsProcessed (fromList [commandId]) ,
                                                       aggregateId,
                                                       state = Just $ GsdState {goals = updateGoalStatus goalId Paused goals }}
      Created      -> Reject "Trying to pause a goal that is not started"
      Paused       -> Reject "Trying to pause a goal that is already paused"
      Accomplished -> Reject "Trying to pause a goal that is already accomplished"
      GivenUp      -> Reject "Trying to pause a goal that is given up"

  where
      findGoal :: GoalId -> [Goal] -> Maybe Goal
      findGoal  goalIdToFind goals = find (\Goal{goalId} -> goalIdToFind == goalId ) goals

      updateGoalStatus :: GoalId -> GoalStatus -> [Goal] -> [Goal]
      updateGoalStatus goalIdToUpdate newGoalStatus goals =
        map (\goal@Goal{workspaceId,goalId,actions,description} -> case (goalIdToUpdate == goalId) of
          True -> Goal{workspaceId,goalId, actions, description, status = newGoalStatus}
          False -> goal
        ) $ goals