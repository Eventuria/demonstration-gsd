{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Eventuria.GSD.Write.CommandConsumer.Handling.Commands.SetGoal where

import           Data.List hiding (union)
import           Data.Text hiding (map,find,empty)
import qualified Data.UUID.V4 as Uuid
import qualified Data.Time as Time

import           Eventuria.Libraries.PersistedStreamEngine.Interface.Offset

import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandHandlingResult
                 
import           Eventuria.GSD.Write.Model.Events.Event
import           Eventuria.GSD.Write.Model.WriteModel
import           Eventuria.GSD.Write.Model.Core


handle :: Offset ->
          GsdWriteModel ->
          CommandId ->
          WorkspaceId ->
          GoalId ->
          Text  ->
          IO (CommandHandlingResult)
handle offset
       writeModel @ GsdWriteModel {goals}
       commandId
       workspaceId
       goalId
       goalDescription =
  case (isGoalFound goalId goals) of
    True ->  return $ CommandRejected "You can't set the same goal more than once"
    False -> do
       createdOn <- Time.getCurrentTime
       eventId <- Uuid.nextRandom
       return $ CommandValidated [toEvent GoalSet { eventId ,
                                                    createdOn,
                                                    workspaceId ,
                                                    goalId ,
                                                    goalDescription}]

  where
      isGoalFound :: GoalId -> [Goal] -> Bool
      isGoalFound  goalIdToRefine goals =
         case (find (\Goal{goalId} -> goalIdToRefine == goalId ) goals) of
            Just goal -> True
            Nothing -> False
