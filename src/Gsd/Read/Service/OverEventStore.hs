{-# LANGUAGE FlexibleContexts #-}
module Gsd.Read.Service.OverEventStore where

import qualified Gsd.Read.Service.Generic  as GenericRead
import PersistedStreamEngine.Interface.Streamable

import PersistedStreamEngine.Interface.PersistedItem
import PersistedStreamEngine.Instances.EventStore.Client.Dependencies
import PersistedStreamEngine.Instances.EventStore.Read.CqrsInstance

import Gsd.Write.Repository.EventStoreStreams
import Gsd.Write.Model.Core
import Gsd.Read.Model.Workspace
import Gsd.Read.Model.Goal
import Gsd.Read.Model.Action
import CQRS.Write.StreamRepository
import CQRS.Write.Aggregate.Events.Event
import Streamly (SerialT)
import Eventuria.Commons.System.SafeResponse

streamWorkspace :: (Streamable stream monad WorkspaceId , Streamable SerialT monad Event) =>
                      Dependencies ->
                      stream monad (SafeResponse (Persisted Workspace))
streamWorkspace settings =
    GenericRead.streamWorkspace
      (aggregateIdStream $ getEventStoreStreamRepository settings)
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming

fetchWorkspace :: Dependencies -> WorkspaceId -> IO (SafeResponse (Maybe Workspace))
fetchWorkspace settings workspaceId =
    GenericRead.fetchWorkspace
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId

streamGoal :: Streamable stream monad Event =>
                Dependencies ->
                WorkspaceId ->
                stream monad (SafeResponse Goal)
streamGoal settings workspaceId =
    GenericRead.streamGoal
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId

fetchGoal :: Dependencies ->
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
                  Dependencies ->
                  WorkspaceId ->
                  GoalId ->
                  stream monad (SafeResponse (Action))
streamAction settings workspaceId goalId =
    GenericRead.streamAction
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId
      goalId
