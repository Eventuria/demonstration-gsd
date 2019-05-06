{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Eventuria.GSD.Write.CommandConsumer.Handling.Commands.RefineGoalDescription where

import           Data.List (find)

import qualified Data.UUID.V4 as Uuid
import qualified Data.Time as Time


                 
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandHandlingResult
import           Eventuria.GSD.Write.Model.Events.Event
import           Eventuria.GSD.Write.Model.WriteModel
import           Eventuria.GSD.Write.Model.Core
import           Eventuria.GSD.Write.Model.Commands.Command

handle :: GsdWriteModel ->
          RefineGoalDescription  ->
          IO (CommandHandlingResult)
handle writeModel @ GsdWriteModel {goals}
       RefineGoalDescription {commandId, workspaceId, goalId, refinedGoalDescription} =
  case (isGoalFound goalId goals) of
    False -> return $ CommandRejected "Refining a goal description for a goal that does not exist."
    True -> do
      createdOn <- Time.getCurrentTime
      eventId <- Uuid.nextRandom
      return $ CommandValidated [toEvent $ GoalDescriptionRefined {
                                              eventId ,
                                              createdOn,
                                              workspaceId ,
                                              goalId ,
                                              refinedGoalDescription}]
  where
    isGoalFound :: GoalId -> [Goal] -> Bool
    isGoalFound  goalIdToRefine goals = case (find (\Goal{goalId} -> goalIdToRefine == goalId ) goals) of
                                  Just goal -> True
                                  Nothing -> False

