{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Eventuria.GSD.Write.CommandConsumer.Handling.Commands.PauseWorkingOnGoal where

import           Data.List (find)
import qualified Data.UUID.V4 as Uuid
import qualified Data.Time as Time

import           Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandHandlingResult

import           Eventuria.GSD.Write.Model.Events.Event
import           Eventuria.GSD.Write.Model.WriteModel
import           Eventuria.GSD.Write.Model.Core
import           Eventuria.GSD.Write.Model.Commands.Command

handle :: GsdWriteModel ->
          PauseWorkingOnGoal ->
          IO (CommandHandlingResult)
handle writeModel @ GsdWriteModel {goals}
       PauseWorkingOnGoal { commandId, workspaceId, goalId} =
  case (findGoal goalId goals)  of
    Nothing -> return $ CommandRejected  "Trying to pause a goal that does not exist"
    Just goal @ Goal {workspaceId,goalId,description,status} ->
      case status of
        Created      -> return $ CommandRejected  "Trying to pause a goal that is not started"
        Paused       -> return $ CommandRejected  "Trying to pause a goal that is already paused"
        Accomplished -> return $ CommandRejected  "Trying to pause a goal that is already accomplished"
        GivenUp      -> return $ CommandRejected  "Trying to pause a goal that is given up"
        InProgress -> do
          createdOn <- Time.getCurrentTime
          eventId <- Uuid.nextRandom
          return $ CommandValidated [toEvent $ GoalPaused { eventId ,
                                                            createdOn,
                                                            workspaceId ,
                                                            goalId}]
  where
      findGoal :: GoalId -> [Goal] -> Maybe Goal
      findGoal  goalIdToFind goals = find (\Goal{goalId} -> goalIdToFind == goalId ) goals
