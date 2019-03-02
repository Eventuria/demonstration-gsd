{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Eventuria.GSD.Write.CommandConsumer.Handling.Commands.SetGoal where

import Data.Set (fromList,union,empty)
import Data.List hiding (union)
import Data.Text hiding (map,find,empty)

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
          Text  ->
          CommandHandlingResponse GsdState
handle offset
       ValidationState {commandsProcessed, aggregateId,state}
       commandId
       workspaceId
       goalId
       goalDescription =
  case (state) of
    Nothing ->
      ValidateCommandWithFollowingTransactionPayload $ do
          createdOn <- getCurrentTime
          eventId <- getNewEventID
          persistEvent $ toEvent $ GoalSet {eventId ,
                                            createdOn,
                                            workspaceId ,
                                            goalId ,
                                            goalDescription}
          updateValidationState ValidationState {lastOffsetConsumed = offset ,
                                                 commandsProcessed = union commandsProcessed (fromList [commandId]) ,
                                                 aggregateId,
                                                 state = Just $ GsdState {goals = [Goal{workspaceId,
                                                                                        goalId,
                                                                                        description = goalDescription,
                                                                                        actions = empty ,
                                                                                        status = Created}]}}
    Just GsdState {goals} ->
      case (isGoalFound goalId goals) of
        True ->  RejectCommand "You can't set the same goal more than once"
        False -> ValidateCommandWithFollowingTransactionPayload $ do
            createdOn <- getCurrentTime
            eventId <- getNewEventID
            persistEvent $ toEvent $ GoalSet { eventId ,
                                               createdOn,
                                               workspaceId ,
                                               goalId ,
                                               goalDescription}
            updateValidationState ValidationState {lastOffsetConsumed = offset ,
                                                   commandsProcessed = union commandsProcessed (fromList [commandId]) ,
                                                   aggregateId,
                                                   state = Just $ GsdState {goals = (goals ++ [Goal{workspaceId,
                                                                                                    goalId,
                                                                                                    description = goalDescription,
                                                                                                    actions = empty ,
                                                                                                    status = Created}])}}

  where
      isGoalFound :: GoalId -> [Goal] -> Bool
      isGoalFound  goalIdToRefine goals =
         case (find (\Goal{goalId} -> goalIdToRefine == goalId ) goals) of
            Just goal -> True
            Nothing -> False
