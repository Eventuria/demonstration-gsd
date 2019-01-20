{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Gsd.Read.GenericRead (streamWorkspace,streamGoal,streamAction) where

import Streamly hiding (Streaming)
import Data.Function ((&))
import qualified Streamly.Prelude as S
import Data.Maybe
import Data.Text hiding (map,length)

import PersistedStreamEngine.Interface.Streamable
import Cqrs.Write.StreamRepository
import PersistedStreamEngine.Interface.PersistedItem
import Control.Monad.IO.Class (MonadIO(liftIO))

import PersistedStreamEngine.Interface.Read.Reading
import Gsd.Read.Workspace
import Gsd.Read.Goal
import Gsd.Read.Action
import Gsd.Write.Core
import Gsd.Write.Events.Event
import Cqrs.Write.Aggregate.Events.Event
import Cqrs.Write.Serialization.Event ()


data WorkspaceBuilder = WorkspaceBuilder {  workspaceIdMaybe :: Maybe WorkspaceId ,
                                            workspaceNameMaybe :: Maybe WorkspaceName ,
                                            goalStats :: GoalStats,
                                            actionStats :: ActionStats  } deriving Show

streamWorkspace :: (Streamable stream monad WorkspaceId , Streamable SerialT monad Event) =>
                     AggregateIdStream persistedStream ->
                     GetEventStream persistedStream ->
                     Streaming persistedStream ->
                     stream monad (Persisted Workspace)
streamWorkspace aggregateIdStream getEventStream streaming @ Streaming {streamAll} =
    (streamAll $ aggregateIdStream)
      & S.mapM (\PersistedItem { offset = offset, item = workspaceId} ->
        S.foldx
         foldEvent
         WorkspaceBuilder { workspaceIdMaybe = Nothing, workspaceNameMaybe = Nothing,
                            goalStats = GoalStats {total = 0, accomplished = 0, toBeAccomplished = 0 },
                            actionStats = ActionStats {total = 0, completed = 0, opened = 0 } }
         (\WorkspaceBuilder{workspaceIdMaybe,workspaceNameMaybe, goalStats, actionStats} ->
            case (workspaceIdMaybe, workspaceNameMaybe) of
              (Just workspaceId, Just workspaceName) ->
                 Just PersistedItem {
                    offset = offset,
                    item = Workspace {
                         workspaceId = fromJust $ workspaceIdMaybe,
                         workspaceName = fromJust $ workspaceNameMaybe,
                         goalStats,actionStats}}
              otherwise -> Nothing)
         (streamingEvent getEventStream streaming workspaceId))
      & S.filter (\maybePersistedWorkspace -> case maybePersistedWorkspace of
        Just persistedWorkspace -> True
        Nothing -> False )
      & S.map (\maybePersistedWorkspace -> fromJust $ maybePersistedWorkspace)
  where

    streamingEvent :: (Streamable stream monad Event) =>
                        GetEventStream persistedStream ->
                        Streaming persistedStream ->
                        WorkspaceId ->
                        stream monad GsdEvent
    streamingEvent getEventStream Streaming {streamAll} workspaceId =
        (streamAll $ getEventStream workspaceId) & S.map (\PersistedItem{item = event} -> fromEvent event)

    foldEvent :: WorkspaceBuilder -> GsdEvent -> WorkspaceBuilder
    foldEvent workspaceBuilder gsdEvent =
      case (workspaceBuilder,gsdEvent) of
         (WorkspaceBuilder {..} , WorkspaceCreated {workspaceId}) ->
            WorkspaceBuilder { workspaceIdMaybe = Just workspaceId, ..}
         (WorkspaceBuilder {..} , WorkspaceNamed {workspaceName}) ->
            WorkspaceBuilder { workspaceNameMaybe = Just workspaceName, ..}
         (WorkspaceBuilder {..} , WorkspaceRenamed {workspaceNewName}) ->
            WorkspaceBuilder { workspaceNameMaybe = Just workspaceNewName, ..}
         (WorkspaceBuilder {goalStats = GoalStats {total,toBeAccomplished, ..}, .. }, GoalSet {}) ->
            WorkspaceBuilder { goalStats =  GoalStats {
                                                total = total + 1,
                                                toBeAccomplished = toBeAccomplished +1 , ..} , ..}
         (WorkspaceBuilder {goalStats = GoalStats {total,toBeAccomplished, ..}, .. }, GoalGivenUp {}) ->
            WorkspaceBuilder { goalStats =  GoalStats {
                                                total = total + 1,
                                                toBeAccomplished = toBeAccomplished -1 , ..} , ..}
         (WorkspaceBuilder {goalStats = GoalStats {total,toBeAccomplished, accomplished}, ..} , GoalAccomplished {}) ->
            WorkspaceBuilder { goalStats =  GoalStats {
                                                total = total + 1,
                                                toBeAccomplished = toBeAccomplished -1,
                                                accomplished = accomplished +1 } , ..}
         (WorkspaceBuilder {actionStats = ActionStats {total,opened, ..}, .. } , ActionRevealed  {}) ->
            WorkspaceBuilder { actionStats =  ActionStats {
                                                total = total + 1,
                                                opened = opened +1, ..} , ..}
         (WorkspaceBuilder {actionStats = ActionStats {total,opened, completed}, .. } , ActionCompleted  {}) ->
            WorkspaceBuilder { actionStats =  ActionStats {
                                                total = total + 1,
                                                opened = opened -1,
                                                completed = completed +1} , ..}
         (workspaceBuilder , _ ) -> workspaceBuilder



streamGoal :: Streamable stream monad Event =>
                GetEventStream persistedStream ->
                Streaming persistedStream ->
                WorkspaceId ->
                stream monad Goal
streamGoal getEventStream streaming workspaceId = do
    goals <- liftIO $ S.foldx
      folding
      []
      id
      (streamingEvent getEventStream streaming workspaceId)
    S.fromList goals
  where
    streamingEvent :: (Streamable stream monad Event) =>
                        GetEventStream persistedStream ->
                        Streaming persistedStream ->
                        WorkspaceId ->
                        stream monad GsdEvent
    streamingEvent getEventStream Streaming {streamAll} workspaceId =
      (streamAll $ getEventStream workspaceId) & S.map (\PersistedItem{item = event} -> fromEvent event)

    folding :: [Goal] -> GsdEvent -> [Goal]
    folding goals GoalSet {workspaceId, goalId, goalDescription} =
      goals ++ [Goal { workspaceId, goalId, description = goalDescription, status = Created}]
    folding goals GoalDescriptionRefined {goalId, refinedGoalDescription} =
      updateGoals goalId refinedGoalDescription goals
      where
        updateGoals :: GoalId -> Text -> [Goal] -> [Goal]
        updateGoals goalIdToUpdate refinedGoalDescription goals =
          map (\goal@Goal{workspaceId,goalId,status} -> case (goalIdToUpdate == goalId) of
            True -> Goal{workspaceId,goalId, description = refinedGoalDescription, status}
            False -> goal
          ) $ goals
    folding goals GoalStarted {goalId} = updateGoalStatus goalId InProgress goals
    folding goals GoalPaused {goalId} = updateGoalStatus goalId Paused goals
    folding goals GoalAccomplished {goalId} = updateGoalStatus goalId Accomplished goals
    folding goals GoalGivenUp {goalId} = updateGoalStatus goalId GivenUp goals
    folding goals gsdEvent = goals

    updateGoalStatus :: GoalId -> GoalStatus -> [Goal] -> [Goal]
    updateGoalStatus goalIdToUpdate newGoalStatus goals =
      map (\goal@Goal{workspaceId,goalId,description} -> case (goalIdToUpdate == goalId) of
        True -> Goal{workspaceId,goalId, description, status = newGoalStatus}
        False -> goal
      ) $ goals


streamAction :: Streamable stream monad Event =>
                  GetEventStream persistedStream ->
                  Streaming persistedStream ->
                  WorkspaceId ->
                  GoalId ->
                  stream monad Action
streamAction getEventStream streaming workspaceId goalId = do
    actions <- liftIO $ S.foldx
      folding
      []
      id
      (streamingEvent getEventStream streaming workspaceId goalId)

    S.fromList actions
  where
    streamingEvent :: (Streamable stream monad Event) =>
                        GetEventStream persistedStream ->
                        Streaming persistedStream ->
                        WorkspaceId -> GoalId ->
                        stream monad GsdEvent
    streamingEvent getEventStream Streaming {streamAll} workspaceId goalIdToStream =
      (streamAll $ getEventStream workspaceId) & S.map (\PersistedItem{item = event} -> fromEvent event)
          & S.filter (\gsdEvent ->
                                case gsdEvent of
                                    ActionRevealed {goalId} -> goalId == goalIdToStream
                                    ActionCompleted {goalId} -> goalId == goalIdToStream
                                    _ -> False)

    folding :: [Action] -> GsdEvent -> [Action]
    folding actions ActionRevealed {actionDetails, ..} =
      actions ++ [Action { indexation = (length actions), details = actionDetails, status = Initiated, ..}]
    folding actions ActionCompleted {workspaceId,goalId,actionId} = updateActions actionId Completed actions
      where
        updateActions :: ActionId -> ActionStatus -> [Action] -> [Action]
        updateActions actionIdToUpdate actionStatus actions =
          map (\action@Action{..} -> case (actionIdToUpdate == actionId) of
            True -> Action{status = actionStatus, ..}
            False -> action
          ) $ actions
    folding actions gsdEvent = actions






