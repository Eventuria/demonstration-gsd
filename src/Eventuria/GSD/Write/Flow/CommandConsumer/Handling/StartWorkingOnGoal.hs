{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Eventuria.GSD.Write.Flow.CommandConsumer.Handling.StartWorkingOnGoal where


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
  Nothing -> Reject "Trying to start a goal but there are no goals in that workspace"
  Just GsdState {goals} -> case (findGoal goalId goals)  of
    Nothing -> Reject "Trying to start a goal that does not exist"
    Just goal @ Goal {workspaceId,goalId,description,status} -> case status of
      Created ->
        Validate $ do
                now <- getCurrentTime
                eventId <- getNewEventID
                persistEvent $ toEvent $ GoalStarted {  eventId , createdOn = now, workspaceId , goalId}
                updateValidationState ValidationState {lastOffsetConsumed = offset ,
                                                       commandsProcessed = union commandsProcessed (fromList [commandId]) ,
                                                       aggregateId,
                                                       state = Just $ GsdState {goals = updateGoalStatus goalId InProgress goals }}
      _ -> Reject "Trying to start a goal that is already started"


  where
      findGoal :: GoalId -> [Goal] -> Maybe Goal
      findGoal  goalIdToFind goals = find (\Goal{goalId} -> goalIdToFind == goalId ) goals

      updateGoalStatus :: GoalId -> GoalStatus -> [Goal] -> [Goal]
      updateGoalStatus goalIdToUpdate newGoalStatus goals =
        map (\goal@Goal{workspaceId,goalId,description,actions} -> case (goalIdToUpdate == goalId) of
          True -> Goal{workspaceId,goalId, description, actions, status = newGoalStatus}
          False -> goal
        ) $ goals