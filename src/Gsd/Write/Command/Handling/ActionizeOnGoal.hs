{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Gsd.Write.Command.Handling.ActionizeOnGoal where


import CQRS.EDsl hiding (Action)
import Gsd.Write.Model.Events.Event
import Gsd.Write.Model.State
import CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import Gsd.Write.Model.Core
import Data.Text hiding (map,find)
import CQRS.Write.Aggregate.Commands.CommandId
import PersistedStreamEngine.Interface.Offset
import Data.Set hiding (map)
import Data.List hiding (union)


handle :: Offset ->
          ValidationState GsdState ->
          CommandId ->
          WorkspaceId ->
          GoalId ->
          ActionId ->
          Text  ->
          CommandDirective GsdState
handle offset
       ValidationState {commandsProcessed, aggregateId,state}
       commandId workspaceId goalId actionId actionDetails = case (state) of
  Nothing -> Reject "Trying to actionize on a goal but there is no goal in the workspace given"
  Just GsdState {goals} -> case (findGoal goalId goals)  of
    Nothing -> Reject "Trying to actionize on a goal that does not exist"
    Just goal @ Goal {workspaceId,goalId, actions,  description,status} -> case (findAction actionId actions)  of
        Just action -> Reject "Trying to actionize on a goal more than once"
        Nothing -> Validate $ do
          now <- getCurrentTime
          eventId <- getNewEventID
          persistEvent $ toEvent $ ActionRevealed {  eventId , createdOn = now, workspaceId , goalId , actionId, actionDetails}
          updateValidationState ValidationState {lastOffsetConsumed = offset ,
                                                 commandsProcessed = union commandsProcessed (fromList [commandId]) ,
                                                 aggregateId,
                                                 state = Just $ GsdState {goals = addAction goalId (Action {actionId, index = size actions, details = actionDetails,status = Initiated}) goals }}

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
