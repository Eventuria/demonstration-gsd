{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Eventuria.GSD.Write.CommandConsumer.Handling.Commands.GiveUpOnGoal where

import Data.Text hiding (map,find)
import Data.Set  hiding (map)
import Data.List (find)

import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset

import Eventuria.Libraries.CQRS.Write.CommandConsumption.Handling.ResponseDSL
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId

import Eventuria.GSD.Write.Model.Events.Event
import Eventuria.GSD.Write.Model.State
import Eventuria.GSD.Write.Model.Core


handle :: Offset ->
          ValidationState GsdState ->
          CommandId ->
          WorkspaceId ->
          GoalId ->
          Text ->
          CommandHandlingResponse GsdState
handle offset
       ValidationState {commandsProcessed, aggregateId, state}
       commandId
       workspaceId
       goalId
       reasonToGiveUp =
  case state of
    Nothing -> RejectCommand "Trying to give up a goal but there are no goals in that workspace"
    Just GsdState {goals} ->
      case (findGoal goalId goals)  of
        Nothing -> RejectCommand "Trying to pause a goal that does not exist"
        Just goal @ Goal {workspaceId,goalId,description,status} ->
          case status of
              Accomplished -> RejectCommand "Trying to give up a goal that is already accomplished"
              GivenUp      -> RejectCommand "Trying to give up a goal that is already given up"
              InProgress   -> validateCommand goals reasonToGiveUp
              Created      -> validateCommand goals reasonToGiveUp
              Paused       -> validateCommand goals reasonToGiveUp



  where
      validateCommand :: [Goal] ->
                         Text ->
                         CommandHandlingResponse GsdState
      validateCommand goals
                      reasonToGiveUp  =
        ValidateCommandWithFollowingTransactionPayload $ do
          createdOn <- getCurrentTime
          eventId <- getNewEventID
          persistEvent $ toEvent $ GoalGivenUp {
                                      eventId ,
                                      createdOn,
                                      workspaceId ,
                                      goalId,
                                      reason = reasonToGiveUp}
          updateValidationState ValidationState {
                                  lastOffsetConsumed = offset ,
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