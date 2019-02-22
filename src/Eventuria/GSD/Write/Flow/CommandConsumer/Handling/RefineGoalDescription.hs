{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Eventuria.GSD.Write.Flow.CommandConsumer.Handling.RefineGoalDescription where


import Eventuria.Libraries.CQRS.EDsl
import Eventuria.GSD.Write.Model.Events.Event
import Eventuria.GSD.Write.Model.State
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import Eventuria.GSD.Write.Model.Core
import Data.Text hiding (find,map)
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import Data.Set hiding (map)
import Data.List (find)

handle :: Offset -> ValidationState GsdState -> CommandId -> WorkspaceId -> GoalId -> Text  -> CommandDirective GsdState
handle offset ValidationState {commandsProcessed, aggregateId, state} commandId workspaceId goalId refinedGoalDescription =
  case state of
    Nothing -> Reject "Refining a goal description for a goal that does not exist."
    Just GsdState {goals} -> case (isGoalFound goalId goals) of
      False -> Reject "Refining a goal description for a goal that does not exist."
      True -> Validate $ do
        now <- getCurrentTime
        eventId <- getNewEventID
        persistEvent $ toEvent $ GoalDescriptionRefined {  eventId , createdOn = now, workspaceId , goalId , refinedGoalDescription}
        updateValidationState ValidationState {lastOffsetConsumed = offset ,
                                               commandsProcessed = union commandsProcessed (fromList [commandId]) ,
                                               aggregateId,
                                               state = Just $ GsdState {goals = updateGoals goalId refinedGoalDescription goals }}

  where
    isGoalFound :: GoalId -> [Goal] -> Bool
    isGoalFound  goalIdToRefine goals = case (find (\Goal{goalId} -> goalIdToRefine == goalId ) goals) of
                                  Just goal -> True
                                  Nothing -> False

    updateGoals :: GoalId -> Text -> [Goal] -> [Goal]
    updateGoals goalIdToUpdate refinedGoalDescription goals =
      map (\goal@Goal{workspaceId,goalId,actions,status} -> case (goalIdToUpdate == goalId) of
        True -> Goal{workspaceId,goalId, actions,description = refinedGoalDescription,status}
        False -> goal
      ) $ goals