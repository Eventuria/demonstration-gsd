{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Eventuria.GSD.Write.CommandConsumer.Handling.Commands.NotifyActionCompleted where

import qualified Data.Set as Set
import qualified Data.UUID.V4 as Uuid
import qualified Data.Time as Time
import           Data.List hiding (union)

import           Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandHandlingResult

import           Eventuria.GSD.Write.Model.Events.Event
import           Eventuria.GSD.Write.Model.WriteModel
import           Eventuria.GSD.Write.Model.Core
import           Eventuria.GSD.Write.Model.Commands.Command

handle :: GsdWriteModel -> NotifyActionCompleted -> IO (CommandHandlingResult)
handle writeModel @ GsdWriteModel {goals}
       NotifyActionCompleted {commandId, workspaceId, goalId, actionId} =
  case (findGoal goalId goals)  of
    Nothing -> return $ CommandRejected  "Trying to notify an action completion on a goal that does not exist"
    Just goal @ Goal {workspaceId,goalId, actions,  description,status} ->
      case (findAction actionId actions)  of
        Nothing -> return $ CommandRejected  "Trying to notify an action completion on a action that does not exist"
        Just action @ Action {actionId,index,details,status}  ->
          case status of
            Completed -> return $ CommandRejected  "Trying to notify an action completion on a action already completed"
            Initiated -> do
               createdOn <- Time.getCurrentTime
               eventId <- Uuid.nextRandom
               return $ CommandValidated [toEvent $ ActionCompleted {
                                               eventId ,
                                               createdOn,
                                               workspaceId ,
                                               goalId ,
                                               actionId}]

  where
      findGoal :: GoalId -> [Goal] -> Maybe Goal
      findGoal  goalIdToFind goals = find (\Goal{goalId} -> goalIdToFind == goalId ) goals

      findAction :: ActionId -> Set.Set Action -> Maybe Action
      findAction  actionIdToFind actions = find (\Action{actionId} -> actionIdToFind == actionId ) actions



