{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Eventuria.GSD.Write.Flow.CommandConsumer.Handling.SetGoal where


import Eventuria.Libraries.CQRS.EDsl
import Eventuria.GSD.Write.Model.Events.Event
import Eventuria.GSD.Write.Model.State
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import Eventuria.GSD.Write.Model.Core
import Data.Text hiding (map,find,empty)
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import Data.Set (fromList,union,empty)
import Data.List hiding (union)


handle :: Offset -> ValidationState GsdState -> CommandId -> WorkspaceId -> GoalId -> Text  -> CommandDirective GsdState
handle offset ValidationState {commandsProcessed, aggregateId,state} commandId workspaceId goalId goalDescription = case (state) of
  Nothing ->
    Validate $ do
        now <- getCurrentTime
        eventId <- getNewEventID
        persistEvent $ toEvent $ GoalSet {  eventId , createdOn = now, workspaceId , goalId , goalDescription}
        updateValidationState ValidationState {lastOffsetConsumed = offset ,
                                               commandsProcessed = union commandsProcessed (fromList [commandId]) ,
                                               aggregateId,
                                               state = Just $ GsdState {goals = [Goal{workspaceId,goalId,description = goalDescription, actions = empty , status = Created}]}}
  Just GsdState {goals} -> case (isGoalFound goalId goals) of
    True ->  Reject "You can't set the same goal more than once"
    False -> Validate $ do
        now <- getCurrentTime
        eventId <- getNewEventID
        persistEvent $ toEvent $ GoalSet {  eventId , createdOn = now, workspaceId , goalId , goalDescription}
        updateValidationState ValidationState {lastOffsetConsumed = offset ,
                                               commandsProcessed = union commandsProcessed (fromList [commandId]) ,
                                               aggregateId,
                                               state = Just $ GsdState {goals = (goals ++ [Goal{workspaceId,goalId,description = goalDescription,actions = empty ,status = Created}])}}

  where
      isGoalFound :: GoalId -> [Goal] -> Bool
      isGoalFound  goalIdToRefine goals = case (find (\Goal{goalId} -> goalIdToRefine == goalId ) goals) of
                                    Just goal -> True
                                    Nothing -> False
