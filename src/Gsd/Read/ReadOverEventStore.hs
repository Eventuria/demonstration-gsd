{-# LANGUAGE FlexibleContexts #-}
module Gsd.Read.ReadOverEventStore where

import qualified Gsd.Read.GenericRead  as GenericRead
import PersistedStreamEngine.Interface.Streamable

import PersistedStreamEngine.Interface.PersistedItem
import PersistedStreamEngine.Instances.EventStore.EventStoreClientState
import PersistedStreamEngine.Instances.EventStore.Read.CqrsInstance

import Gsd.Write.EventStoreStreamRepository
import Gsd.Write.Core
import Gsd.Read.Workspace
import Gsd.Read.Goal
import Gsd.Read.Action
import Cqrs.Write.StreamRepository
import Cqrs.Write.Aggregate.Events.Event
import Streamly (SerialT)
import System.SafeResponse

streamWorkspace :: (Streamable stream monad WorkspaceId , Streamable SerialT monad Event) =>
                      EventStoreClientState ->
                      stream monad (SafeResponse (Persisted Workspace))
streamWorkspace settings =
    GenericRead.streamWorkspace
      (aggregateIdStream $ getEventStoreStreamRepository settings)
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming

fetchWorkspace :: EventStoreClientState -> WorkspaceId -> IO (SafeResponse (Maybe Workspace))
fetchWorkspace settings workspaceId =
    GenericRead.fetchWorkspace
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId

streamGoal :: Streamable stream monad Event =>
                EventStoreClientState ->
                WorkspaceId ->
                stream monad (SafeResponse Goal)
streamGoal settings workspaceId =
    GenericRead.streamGoal
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId

fetchGoal :: EventStoreClientState ->
             WorkspaceId ->
             GoalId ->
             IO (SafeResponse (Maybe Goal))
fetchGoal settings workspaceId goalId =
    GenericRead.fetchGoal
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId
      goalId

streamAction :: Streamable stream monad Event =>
                  EventStoreClientState ->
                  WorkspaceId ->
                  GoalId ->
                  stream monad (SafeResponse (Action))
streamAction settings workspaceId goalId =
    GenericRead.streamAction
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId
      goalId
