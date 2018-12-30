{-# LANGUAGE FlexibleContexts #-}
module Gsd.Read.ReadOverEventStore (streamWorkspaces) where

import qualified Gsd.Read.GenericRead  as GenericRead
import PersistedStreamEngine.Interface.Streamable

import PersistedStreamEngine.Interface.PersistedItem
import PersistedStreamEngine.Instances.EventStore.EventStoreSettings
import PersistedStreamEngine.Instances.EventStore.Read.CqrsInstance

import Gsd.Write.EventStoreStreamRepository
import Gsd.Write.Core
import Gsd.Read.Workspace
import Cqrs.Write.StreamRepository
import Cqrs.Write.Aggregate.Events.Event
import Streamly (SerialT)

streamWorkspaces :: (Streamable stream monad WorkspaceId , Streamable SerialT monad Event) => EventStoreSettings -> stream monad (Persisted Workspace)
streamWorkspaces settings =
    GenericRead.streamWorkspaces
      (aggregateIdStream $ getEventStoreStreamRepository settings)
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
