{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Gsd.Write.Command.Handling.GiveUpOnGoal where


import CQRS.EDsl
import Gsd.Write.Model.Events.Event
import Gsd.Write.Model.State
import CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import Gsd.Write.Model.Core
import Data.Text hiding (map,find)
import CQRS.Write.Aggregate.Commands.CommandId
import PersistedStreamEngine.Interface.Offset
import Data.Set hiding (map)

import Data.List (find)

handle :: Offset -> ValidationState GsdState -> CommandId -> WorkspaceId -> GoalId -> Text -> CommandDirective GsdState
handle offset ValidationState {commandsProcessed, aggregateId, state} commandId workspaceId goalId reasonToGiveUp= case state of
  Nothing -> Reject "Trying to give up a goal but there are no goals in that workspace"
  Just GsdState {goals} -> case (findGoal goalId goals)  of
    Nothing -> Reject "Trying to pause a goal that does not exist"
    Just goal @ Goal {workspaceId,goalId,description,status} -> case status of
      Accomplished -> Reject "Trying to give up a goal that is already accomplished"
      GivenUp      -> Reject "Trying to give up a goal that is already given up"
      InProgress   -> validateTransaction goals reasonToGiveUp
      Created      -> validateTransaction goals reasonToGiveUp
      Paused       -> validateTransaction goals reasonToGiveUp



  where
      validateTransaction :: [Goal] -> Text -> CommandDirective GsdState
      validateTransaction goals reasonToGiveUp  =
        Validate $ do
                        now <- getCurrentTime
                        eventId <- getNewEventID
                        persistEvent $ toEvent $ GoalGivenUp {  eventId , createdOn = now, workspaceId , goalId, reason = reasonToGiveUp}
                        updateValidationState ValidationState {lastOffsetConsumed = offset ,
                                                               commandsProcessed = union commandsProcessed (fromList [commandId]) ,
                                                               aggregateId,
                                                               state = Just $ GsdState {goals = updateGoalStatus goalId GivenUp goals }}
      findGoal :: GoalId -> [Goal] -> Maybe Goal
      findGoal  goalIdToFind goals = find (\Goal{goalId} -> goalIdToFind == goalId ) goals

      updateGoalStatus :: GoalId -> GoalStatus -> [Goal] -> [Goal]
      updateGoalStatus goalIdToUpdate newGoalStatus goals =
        map (\goal@Goal{workspaceId,goalId,actions,description} -> case (goalIdToUpdate == goalId) of
          True -> Goal{workspaceId,goalId,actions, description, status = newGoalStatus}
          False -> goal
        ) $ goals