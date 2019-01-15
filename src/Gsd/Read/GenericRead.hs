{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Gsd.Read.GenericRead (streamWorkspace,streamGoal) where

import Streamly hiding (Streaming)
import Data.Function ((&))
import qualified Streamly.Prelude as S
import Data.Maybe
import Data.Text hiding (map)
import PersistedStreamEngine.Interface.Streamable
import Cqrs.Write.StreamRepository
import PersistedStreamEngine.Interface.PersistedItem
import Control.Monad.IO.Class (MonadIO(liftIO))

import PersistedStreamEngine.Interface.Read.Reading
import Gsd.Read.Workspace
import Gsd.Read.Goal
import Gsd.Write.Core
import Gsd.Write.Events.Event
import Cqrs.Write.Aggregate.Events.Event
import Cqrs.Write.Serialization.Event ()

data WorkspaceBuilder = WorkspaceBuilder { workspaceIdMaybe :: Maybe WorkspaceId , workspaceNameMaybe :: Maybe WorkspaceName } deriving Show

streamWorkspace :: (Streamable stream monad WorkspaceId , Streamable SerialT monad Event) => AggregateIdStream persistedStream -> GetEventStream persistedStream -> Streaming persistedStream -> stream monad (Persisted Workspace)
streamWorkspace aggregateIdStream getEventStream Streaming {streamAll} =
    (streamAll $ aggregateIdStream)
      & S.mapM (\PersistedItem { offset = offset, item = workspaceId} -> do
        S.foldx
           foldEvent
           WorkspaceBuilder { workspaceIdMaybe = Nothing, workspaceNameMaybe = Nothing}
           (\WorkspaceBuilder{workspaceIdMaybe,workspaceNameMaybe} -> PersistedItem {
              offset = offset,
              item = Workspace {
                   workspaceId = fromJust $ workspaceIdMaybe,
                   workspaceName = fromJust $ workspaceNameMaybe}})
           $ (streamAll $ getEventStream workspaceId)
              & S.map (\PersistedItem{item = event} -> fromEvent event)
              & S.filter (\gsdEvent ->
                case gsdEvent of
                    WorkspaceCreated {..} -> True
                    WorkspaceNamed {..} -> True
                    WorkspaceRenamed {..} -> True
                    _ -> False))
    where
          foldEvent :: WorkspaceBuilder -> GsdEvent -> WorkspaceBuilder
          foldEvent workspaceBuilder gsdEvent   =
            case (workspaceBuilder,gsdEvent) of
               (WorkspaceBuilder {workspaceIdMaybe,workspaceNameMaybe} , WorkspaceCreated {workspaceId}) -> WorkspaceBuilder { workspaceIdMaybe = Just workspaceId, workspaceNameMaybe}
               (WorkspaceBuilder {workspaceIdMaybe,workspaceNameMaybe} , WorkspaceNamed {workspaceName}) -> WorkspaceBuilder { workspaceIdMaybe, workspaceNameMaybe = Just workspaceName}
               (WorkspaceBuilder {workspaceIdMaybe,workspaceNameMaybe} , WorkspaceRenamed {workspaceNewName}) -> WorkspaceBuilder { workspaceIdMaybe, workspaceNameMaybe = Just workspaceNewName}
               (workspaceBuilder , _ ) -> workspaceBuilder



streamGoal :: Streamable stream monad Event => GetEventStream persistedStream -> Streaming persistedStream -> WorkspaceId -> stream monad Goal
streamGoal getEventStream streaming workspaceId = do
    goals <- liftIO $ S.foldx
      folding
      []
      id
      (streamingGoalRelatedEvents getEventStream streaming workspaceId)
    S.fromList goals
  where
    streamingGoalRelatedEvents :: (Streamable stream monad Event) => GetEventStream persistedStream -> Streaming persistedStream -> WorkspaceId -> stream monad GsdEvent
    streamingGoalRelatedEvents getEventStream Streaming {streamAll} workspaceId =
      (streamAll $ getEventStream workspaceId) & S.map (\PersistedItem{item = event} -> fromEvent event)

    folding :: [Goal] -> GsdEvent -> [Goal]
    folding goals GoalSet {workspaceId, goalId, goalDescription} = goals ++ [Goal { workspaceId, goalId, description = goalDescription, status = Created}]
    folding goals GoalDescriptionRefined {goalId, refinedGoalDescription} = updateGoals goalId refinedGoalDescription goals
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


