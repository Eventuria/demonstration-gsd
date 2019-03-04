{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Eventuria.GSD.Write.CommandConsumer.Handling.Commands.StartWorkingOnGoal where

import           Data.List (find)
import qualified Data.UUID.V4 as Uuid
import qualified Data.Time as Time

import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.Handling.CommandHandler
                 
import           Eventuria.GSD.Write.Model.Events.Event
import           Eventuria.GSD.Write.Model.WriteModel
import           Eventuria.GSD.Write.Model.Core


handle :: Offset ->
          GsdWriteModel ->
          CommandId ->
          WorkspaceId ->
          GoalId ->
          IO (CommandHandlerResult GsdWriteModel)
handle offset
       writeModel @ GsdWriteModel {goals}
       commandId
       workspaceId
       goalId =
          case (findGoal goalId goals)  of
            Nothing -> return $ rejectCommand (Just writeModel) "Trying to start a goal that does not exist"
            Just goal @ Goal {workspaceId,goalId,description,status} ->
              case status of
                Created -> do
                  createdOn <- Time.getCurrentTime
                  eventId <- Uuid.nextRandom
                  return $ validateCommand
                              GsdWriteModel {goals = updateGoalStatus goalId InProgress goals }
                              [toEvent GoalStarted { eventId ,
                                                      createdOn,
                                                      workspaceId ,
                                                      goalId}]
                _ -> return $ rejectCommand (Just writeModel) "Trying to start a goal that is already started"


  where
      findGoal :: GoalId -> [Goal] -> Maybe Goal
      findGoal  goalIdToFind goals = find (\Goal{goalId} -> goalIdToFind == goalId ) goals

      updateGoalStatus :: GoalId -> GoalStatus -> [Goal] -> [Goal]
      updateGoalStatus goalIdToUpdate newGoalStatus goals =
        map (\goal@Goal{workspaceId,goalId,description,actions} -> case (goalIdToUpdate == goalId) of
          True -> Goal{workspaceId,goalId, description, actions, status = newGoalStatus}
          False -> goal
        ) $ goals