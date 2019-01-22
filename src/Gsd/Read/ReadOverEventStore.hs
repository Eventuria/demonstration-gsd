{-# LANGUAGE FlexibleContexts #-}
module Gsd.Read.ReadOverEventStore where

import qualified Gsd.Read.GenericRead  as GenericRead
import PersistedStreamEngine.Interface.Streamable

import PersistedStreamEngine.Interface.PersistedItem
import PersistedStreamEngine.Instances.EventStore.EventStoreSettings
import PersistedStreamEngine.Instances.EventStore.Read.CqrsInstance

import Gsd.Write.EventStoreStreamRepository
import Gsd.Write.Core
import Gsd.Read.Workspace
import Gsd.Read.Goal
import Gsd.Read.Action
import Cqrs.Write.StreamRepository
import Cqrs.Write.Aggregate.Events.Event
import Streamly (SerialT)

streamWorkspace :: (Streamable stream monad WorkspaceId , Streamable SerialT monad Event) => EventStoreSettings -> stream monad (Persisted Workspace)
streamWorkspace settings =
    GenericRead.streamWorkspace
      (aggregateIdStream $ getEventStoreStreamRepository settings)
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming

fetchWorkspace :: EventStoreSettings -> WorkspaceId -> IO (Maybe Workspace)
fetchWorkspace settings workspaceId =
    GenericRead.fetchWorkspace
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId

streamGoal :: Streamable stream monad Event => EventStoreSettings -> WorkspaceId -> stream monad Goal
streamGoal settings workspaceId =
    GenericRead.streamGoal
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId

streamAction :: Streamable stream monad Event => EventStoreSettings -> WorkspaceId -> GoalId -> stream monad Action
streamAction settings workspaceId goalId =
    GenericRead.streamAction
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId
      goalId
