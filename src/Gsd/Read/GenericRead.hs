{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Read.GenericRead (streamWorkspaces) where

import Streamly hiding (Streaming)
import Data.Function ((&))
import qualified Streamly.Prelude as S
import Data.Maybe

import PersistedStreamEngine.Interface.Streamable
import Cqrs.Write.StreamRepository
import PersistedStreamEngine.Interface.PersistedItem


import PersistedStreamEngine.Interface.Read.Reading
import Gsd.Read.Workspace
import Gsd.Write.Core
import Gsd.Write.Events
import Cqrs.Write.Aggregate.Events.Event

data WorkspaceBuilder = WorkspaceBuilder { workspaceIdMaybe :: Maybe WorkspaceId , workspaceNameMaybe :: Maybe WorkspaceName } deriving Show

streamWorkspaces :: (Streamable stream monad WorkspaceId , Streamable SerialT monad Event) => AggregateIdStream persistedStream -> GetEventStream persistedStream -> Streaming persistedStream -> stream monad (Persisted Workspace)
streamWorkspaces aggregateIdStream getEventStream Streaming {streamAll} =
    (streamAll $ aggregateIdStream)
      & S.mapM (\PersistedItem { offset = offset, item = workspaceId} -> do
          workspaceBuilder <-  S.foldr foldEventsForStreamingWorkspaces WorkspaceBuilder { workspaceIdMaybe = Nothing, workspaceNameMaybe = Nothing}
                                   $ (streamAll $ getEventStream workspaceId)
                                      & S.map (\PersistedItem{item = event} -> fromEvent event)
                                      & S.filter (\gsdEvent ->
                                        case gsdEvent of
                                            WorkspaceCreated {..} -> True
                                            WorkspaceNamed {..} -> True)
          return $ PersistedItem { offset = offset,
                          item = Workspace {
                                workspaceId = fromJust $ workspaceIdMaybe workspaceBuilder ,
                                workspaceName = fromJust $ workspaceNameMaybe workspaceBuilder}})


foldEventsForStreamingWorkspaces :: GsdEvent -> WorkspaceBuilder ->  WorkspaceBuilder
foldEventsForStreamingWorkspaces gsdEvent workspaceBuilder  =
  case (workspaceBuilder,gsdEvent) of
     (WorkspaceBuilder {workspaceIdMaybe,workspaceNameMaybe} , WorkspaceCreated {workspaceId}) -> WorkspaceBuilder { workspaceIdMaybe = Just workspaceId, workspaceNameMaybe}
     (WorkspaceBuilder {workspaceIdMaybe,workspaceNameMaybe} , WorkspaceNamed {workspaceName}) -> WorkspaceBuilder { workspaceIdMaybe, workspaceNameMaybe = Just workspaceName}