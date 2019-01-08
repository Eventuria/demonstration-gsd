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

import PersistedStreamEngine.Interface.Streamable
import Cqrs.Write.StreamRepository
import PersistedStreamEngine.Interface.PersistedItem


import PersistedStreamEngine.Interface.Read.Reading
import Gsd.Read.Workspace
import Gsd.Read.Goal
import Gsd.Write.Core
import Gsd.Write.Events.Event
import Cqrs.Write.Aggregate.Events.Event


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



streamGoal :: (Streamable stream monad Event) => GetEventStream persistedStream -> Streaming persistedStream -> WorkspaceId -> stream monad Goal
streamGoal getEventStream Streaming {streamAll} workspaceId =
  (streamAll $ getEventStream workspaceId)
      & S.map (\PersistedItem{item = event} -> fromEvent event)
      & S.filter (\gsdEvent ->
        case gsdEvent of
            GoalSet {..} -> True
            _ -> False)
      & S.map (\GoalSet {workspaceId, goalId, goalDescription} -> Goal { workspaceId, goalId, description = goalDescription})




