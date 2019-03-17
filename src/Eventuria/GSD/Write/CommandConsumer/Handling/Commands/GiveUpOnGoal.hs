{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Eventuria.GSD.Write.CommandConsumer.Handling.Commands.GiveUpOnGoal where

import           Data.Text hiding (map,find)
import           Data.List (find)
import qualified Data.UUID.V4 as Uuid
import qualified Data.Time as Time

import           Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
                 
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandHandling.Definition
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
                 
import           Eventuria.GSD.Write.Model.Events.Event
import           Eventuria.GSD.Write.Model.WriteModel
import           Eventuria.GSD.Write.Model.Core


handle :: Offset ->
          GsdWriteModel ->
          CommandId ->
          WorkspaceId ->
          GoalId ->
          Text ->
          IO (CommandHandlingResult)
handle offset
       writeModel @ GsdWriteModel {goals}
       commandId
       workspaceId
       goalId
       reasonToGiveUp =
  case (findGoal goalId goals)  of
    Nothing -> return $ CommandRejected  "Trying to pause a goal that does not exist"
    Just goal @ Goal {workspaceId,goalId,description,status} ->
      case status of
          Accomplished -> return $ CommandRejected  "Trying to give up a goal that is already accomplished"
          GivenUp      -> return $ CommandRejected  "Trying to give up a goal that is already given up"
          InProgress   -> validate goals reasonToGiveUp
          Created      -> validate goals reasonToGiveUp
          Paused       -> validate goals reasonToGiveUp

  where
      validate :: [Goal] -> Text -> IO (CommandHandlingResult)
      validate goals reasonToGiveUp  = do
        createdOn <- Time.getCurrentTime
        eventId <- Uuid.nextRandom
        return $ CommandValidated [toEvent $ GoalGivenUp {
                                                eventId ,
                                                createdOn,
                                                workspaceId ,
                                                goalId,
                                                reason = reasonToGiveUp}]

      findGoal :: GoalId -> [Goal] -> Maybe Goal
      findGoal  goalIdToFind goals = find (\Goal{goalId} -> goalIdToFind == goalId ) goals

      