{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Eventuria.GSD.Write.CommandConsumer.Handling.Commands.NotifyGoalAccomplishment where

import Data.Set hiding (map)
import Data.List (find)

import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset

import Eventuria.Libraries.CQRS.Write.CommandConsumption.Handling.ResponseDSL
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState

import Eventuria.GSD.Write.Model.Events.Event
import Eventuria.GSD.Write.Model.State
import Eventuria.GSD.Write.Model.Core



handle :: Offset ->
          ValidationState GsdState ->
          CommandId ->
          WorkspaceId ->
          GoalId ->
          CommandHandlingResponse GsdState
handle offset
       ValidationState {commandsProcessed, aggregateId, state}
       commandId
       workspaceId
       goalId =
  case state of
    Nothing -> RejectCommand "Trying to notify an accomplishment for  a goal but there are no goals in that workspace"
    Just GsdState {goals} ->
      case (findGoal goalId goals)  of
        Nothing -> RejectCommand "Trying to pause a goal that does not exist"
        Just goal @ Goal {workspaceId,goalId,description,status} -> case status of
          Accomplished -> RejectCommand "Trying to notify an accomplishment a goal that is already accomplished"
          GivenUp      -> RejectCommand "Trying to notify an accomplishment a goal that is given up"
          InProgress   -> validateCommand goals
          Created      -> validateCommand goals
          Paused       -> validateCommand goals



  where
      validateCommand :: [Goal] -> CommandHandlingResponse GsdState
      validateCommand goals =
        ValidateCommandWithFollowingTransactionPayload $ do
          createdOn <- getCurrentTime
          eventId <- getNewEventID
          persistEvent $ toEvent $ GoalAccomplished {eventId ,
                                                     createdOn,
                                                     workspaceId ,
                                                     goalId}
          updateValidationState ValidationState {
                                  lastOffsetConsumed = offset ,
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