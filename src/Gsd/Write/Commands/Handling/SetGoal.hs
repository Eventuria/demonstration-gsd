{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Gsd.Write.Commands.Handling.SetGoal where


import Cqrs.EDsl
import Gsd.Write.Events.Event
import Gsd.Write.State
import Cqrs.Write.Aggregate.Commands.ValidationStates.ValidationState
import Gsd.Write.Core
import Data.Text hiding (map,find)
import Cqrs.Write.Aggregate.Commands.CommandId
import PersistedStreamEngine.Interface.Offset
import Data.Set (fromList)
import Data.List

handle :: Offset -> ValidationState GsdState -> CommandId -> WorkspaceId -> GoalId -> Text  -> CommandDirective GsdState
handle offset ValidationState {state} commandId workspaceId goalId goalDescription = case (state) of
  Nothing ->
    Validate $ do
        now <- getCurrentTime
        eventId <- getNewEventID
        persistEvent $ toEvent $ GoalSet {  eventId , createdOn = now, workspaceId , goalId , goalDescription}
        updateValidationState ValidationState {lastOffsetConsumed = offset ,
                                               commandsProcessed = fromList [commandId] ,
                                               aggregateId = workspaceId,
                                               state = Just $ GsdState {goals = [Goal{workspaceId,goalId,description = goalDescription}]}}
  Just GsdState {goals} -> case (isGoalFound goalId goals) of
    True ->  Reject "You can't set the same goal more than once"
    False -> Validate $ do
        now <- getCurrentTime
        eventId <- getNewEventID
        persistEvent $ toEvent $ GoalSet {  eventId , createdOn = now, workspaceId , goalId , goalDescription}
        updateValidationState ValidationState {lastOffsetConsumed = offset ,
                                               commandsProcessed = fromList [commandId] ,
                                               aggregateId = workspaceId,
                                               state = Just $ GsdState {goals = (goals ++ [Goal{workspaceId,goalId,description = goalDescription}])}}

  where
      isGoalFound :: GoalId -> [Goal] -> Bool
      isGoalFound  goalIdToRefine goals = case (find (\Goal{goalId} -> goalIdToRefine == goalId ) goals) of
                                    Just goal -> True
                                    Nothing -> False

      updateGoals :: GoalId -> Text -> [Goal] -> [Goal]
      updateGoals goalIdToUpdate refinedGoalDescription goals =
        map (\goal@Goal{workspaceId,goalId} -> case (goalIdToUpdate == goalId) of
          True -> Goal{workspaceId,goalId, description = refinedGoalDescription}
          False -> goal
        ) $ goals