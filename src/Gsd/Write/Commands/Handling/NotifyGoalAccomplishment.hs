{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Gsd.Write.Commands.Handling.NotifyGoalAccomplishment where


import Cqrs.EDsl
import Gsd.Write.Events.Event
import Gsd.Write.State
import Cqrs.Write.Aggregate.Commands.ValidationStates.ValidationState
import Gsd.Write.Core

import Cqrs.Write.Aggregate.Commands.CommandId
import PersistedStreamEngine.Interface.Offset
import Data.Set hiding (map)

import Data.List (find)

handle :: Offset -> ValidationState GsdState -> CommandId -> WorkspaceId -> GoalId ->  CommandDirective GsdState
handle offset ValidationState {commandsProcessed, aggregateId, state} commandId workspaceId goalId = case state of
  Nothing -> Reject "Trying to notify an accomplishment for  a goal but there are no goals in that workspace"
  Just GsdState {goals} -> case (findGoal goalId goals)  of
    Nothing -> Reject "Trying to pause a goal that does not exist"
    Just goal @ Goal {workspaceId,goalId,description,status} -> case status of
      Accomplished -> Reject "Trying to notify an accomplishment a goal that is already accomplished"
      GivenUp      -> Reject "Trying to notify an accomplishment a goal that is given up"
      InProgress   -> validateTransaction goals
      Created      -> validateTransaction goals
      Paused       -> validateTransaction goals



  where
      validateTransaction :: [Goal] -> CommandDirective GsdState
      validateTransaction goals =
        Validate $ do
                        now <- getCurrentTime
                        eventId <- getNewEventID
                        persistEvent $ toEvent $ GoalAccomplished {  eventId , createdOn = now, workspaceId , goalId}
                        updateValidationState ValidationState {lastOffsetConsumed = offset ,
                                                               commandsProcessed = union commandsProcessed (fromList [commandId]) ,
                                                               aggregateId,
                                                               state = Just $ GsdState {goals = updateGoalStatus goalId Accomplished goals }}
      findGoal :: GoalId -> [Goal] -> Maybe Goal
      findGoal  goalIdToFind goals = find (\Goal{goalId} -> goalIdToFind == goalId ) goals

      updateGoalStatus :: GoalId -> GoalStatus -> [Goal] -> [Goal]
      updateGoalStatus goalIdToUpdate newGoalStatus goals =
        map (\goal@Goal{workspaceId,goalId,actions,description} -> case (goalIdToUpdate == goalId) of
          True -> Goal{workspaceId,goalId,actions, description, status = newGoalStatus}
          False -> goal
        ) $ goals