{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Eventuria.GSD.Write.CommandConsumer.Handling.Commands.ActionizeOnGoal where

import           Data.Text hiding (map,find)
import           Data.Set hiding (map)
import           Data.List hiding (union)
import qualified Data.UUID.V4 as Uuid
import qualified Data.Time as Time

import           Eventuria.Libraries.CQRS.Write.CommandConsumption.Handling.CommandHandler
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
                 
import           Eventuria.GSD.Write.Model.Events.Event
import           Eventuria.GSD.Write.Model.WriteModel
import           Eventuria.GSD.Write.Model.Core


handle :: Offset ->
          GsdWriteModel ->
          CommandId ->
          WorkspaceId ->
          GoalId ->
          ActionId ->
          Text  ->
          IO (CommandHandlerResult GsdWriteModel)
handle offset
       writeModel @ GsdWriteModel {goals}
       commandId workspaceId goalId actionId actionDetails =
  case (findGoal goalId goals)  of
    Nothing -> return $ rejectCommand (Just writeModel) "Trying to actionize on a goal that does not exist"
    Just goal @ Goal {workspaceId,goalId, actions,  description,status} -> case (findAction actionId actions)  of
        Just action -> return $ rejectCommand (Just writeModel) "Trying to actionize on a goal more than once"
        Nothing -> do
          createdOn <- Time.getCurrentTime
          eventId <- Uuid.nextRandom
          return $ validateCommand
                    GsdWriteModel {goals = addAction goalId (Action {actionId,
                                                                     index = size actions,
                                                                     details = actionDetails,
                                                                     status = Initiated}) goals }
                    [toEvent $ ActionRevealed {
                                  eventId ,
                                  createdOn,
                                  workspaceId ,
                                  goalId ,
                                  actionId,
                                  actionDetails}]


  where
      findGoal :: GoalId -> [Goal] -> Maybe Goal
      findGoal  goalIdToFind goals = find (\Goal{goalId} -> goalIdToFind == goalId ) goals

      findAction :: ActionId -> Set Action -> Maybe Action
      findAction  actionIdToFind actions = find (\Action{actionId} -> actionIdToFind == actionId ) actions

      addAction :: GoalId -> Action -> [Goal] -> [Goal]
      addAction goalIdToUpdate action goals =
        map (\goal@Goal{workspaceId,goalId,description, status ,actions} -> case (goalIdToUpdate == goalId) of
          True -> Goal{workspaceId,goalId, description, status , actions = union actions $ fromList [action]}
          False -> goal
        ) $ goals
