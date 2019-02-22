{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Eventuria.GSD.Write.Flow.CommandConsumer.Handling.NotifyActionCompleted where


import Eventuria.Libraries.CQRS.EDsl hiding (Action)
import Eventuria.GSD.Write.Model.Events.Event
import Eventuria.GSD.Write.Model.State
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import Eventuria.GSD.Write.Model.Core
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import qualified Data.Set as Set
import Data.List hiding (union)


handle :: Offset -> ValidationState GsdState -> CommandId -> WorkspaceId -> GoalId -> ActionId ->  CommandDirective GsdState
handle offset ValidationState {commandsProcessed, aggregateId,state} commandId workspaceId goalId actionId = case (state) of
  Nothing -> Reject "Trying to notify an action completion on a goal but there is no goal in the workspace given"
  Just GsdState {goals} -> case (findGoal goalId goals)  of
    Nothing -> Reject "Trying to notify an action completion on a goal that does not exist"
    Just goal @ Goal {workspaceId,goalId, actions,  description,status} -> case (findAction actionId actions)  of
        Nothing -> Reject "Trying to notify an action completion on a action that does not exist"
        Just action @ Action {actionId,index,details,status}  -> case status of
          Completed -> Reject "Trying to notify an action completion on a action already completed"
          Initiated -> Validate $ do
            now <- getCurrentTime
            eventId <- getNewEventID
            persistEvent $ toEvent $ ActionCompleted {  eventId , createdOn = now, workspaceId , goalId , actionId}
            updateValidationState ValidationState {lastOffsetConsumed = offset ,
                                                   commandsProcessed = Set.union commandsProcessed (Set.fromList [commandId]) ,
                                                   aggregateId,
                                                   state = Just $ GsdState {goals = updateGoal goals (updateActions goal Action {actionId,index,details,status = Completed})  }}

  where
      findGoal :: GoalId -> [Goal] -> Maybe Goal
      findGoal  goalIdToFind goals = find (\Goal{goalId} -> goalIdToFind == goalId ) goals

      findAction :: ActionId -> Set.Set Action -> Maybe Action
      findAction  actionIdToFind actions = find (\Action{actionId} -> actionIdToFind == actionId ) actions

      updateActionStatus :: ActionId -> ActionStatus -> Set.Set Action -> Set.Set Action
      updateActionStatus actionIdToUpdate newGoalStatus actions =
        Set.map (\action@Action{actionId, details,index } -> case (actionIdToUpdate == actionId) of
          True -> Action{actionId,details,index, status = Completed}
          False -> action
        ) $ actions

      updateGoal :: [Goal] -> Goal -> [Goal]
      updateGoal goals updatedGoal@Goal{goalId = goalIdToUpdate} =
        map (\goal@Goal{goalId} -> case (goalId == goalIdToUpdate) of
          True -> updatedGoal
          False -> goal
        ) $ goals

      updateActions :: Goal -> Action -> Goal
      updateActions Goal{workspaceId,goalId,description,status,actions} updatedAction @ Action {actionId = actionIdToUpdate} =
        Goal {workspaceId,goalId,description,status,
              actions = Set.map (\action@Action{actionId} -> case (actionId == actionIdToUpdate) of
                                  True -> updatedAction
                                  False -> action
                                ) $ actions}

