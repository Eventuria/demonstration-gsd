{-# LANGUAGE FlexibleContexts #-}
module Gsd.Read.ReadOverEventStore (streamWorkspace,streamGoal) where

import qualified Gsd.Read.GenericRead  as GenericRead
import PersistedStreamEngine.Interface.Streamable

import PersistedStreamEngine.Interface.PersistedItem
import PersistedStreamEngine.Instances.EventStore.EventStoreSettings
import PersistedStreamEngine.Instances.EventStore.Read.CqrsInstance

import Gsd.Write.EventStoreStreamRepository
import Gsd.Write.Core
import Gsd.Read.Workspace
import Gsd.Read.Goal
import Cqrs.Write.StreamRepository
import Cqrs.Write.Aggregate.Events.Event
import Streamly (SerialT)

streamWorkspace :: (Streamable stream monad WorkspaceId , Streamable SerialT monad Event) => EventStoreSettings -> stream monad (Persisted Workspace)
streamWorkspace settings =
    GenericRead.streamWorkspace
      (aggregateIdStream $ getEventStoreStreamRepository settings)
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming

streamGoal :: Streamable stream monad Event => EventStoreSettings -> WorkspaceId -> stream monad Goal
streamGoal settings workspaceId =
    GenericRead.streamGoal
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId
