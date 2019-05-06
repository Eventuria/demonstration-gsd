{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Eventuria.GSD.Write.CommandConsumer.Handling.Commands.StartWorkingOnGoal where

import           Data.List (find)
import qualified Data.UUID.V4 as Uuid
import qualified Data.Time as Time

import           Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandHandlingResult
                 
import           Eventuria.GSD.Write.Model.Events.Event
import           Eventuria.GSD.Write.Model.WriteModel
import           Eventuria.GSD.Write.Model.Core
import           Eventuria.GSD.Write.Model.Commands.Command


handle :: GsdWriteModel ->
          StartWorkingOnGoal ->
          IO (CommandHandlingResult)
handle writeModel @ GsdWriteModel {goals}
       StartWorkingOnGoal {commandId, workspaceId, goalId } =
          case (findGoal goalId goals)  of
            Nothing -> return $ CommandRejected "Trying to start a goal that does not exist"
            Just goal @ Goal {workspaceId,goalId,description,status} ->
              case status of
                Created -> do
                  createdOn <- Time.getCurrentTime
                  eventId <- Uuid.nextRandom
                  return $ CommandValidated [toEvent GoalStarted { eventId ,
                                                                  createdOn,
                                                                  workspaceId ,
                                                                  goalId}]
                _ -> return $ CommandRejected "Trying to start a goal that is already started"


  where
      findGoal :: GoalId -> [Goal] -> Maybe Goal
      findGoal  goalIdToFind goals = find (\Goal{goalId} -> goalIdToFind == goalId ) goals

