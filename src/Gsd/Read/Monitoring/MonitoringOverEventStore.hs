{-# LANGUAGE FlexibleContexts #-}
module Gsd.Read.Monitoring.MonitoringOverEventStore (
                streamCommands,
                streamWorkspaceIds,
                streamInfinitelyCommands) where

import PersistedStreamEngine.Interface.Streamable

import PersistedStreamEngine.Interface.PersistedItem
import Cqrs.Write.Aggregate.Commands.Command

import Gsd.Write.Commands
import Gsd.Write.Core
import Gsd.Write.EventStoreStreamRepository

import qualified Gsd.Read.Monitoring.GenericMonitoring  as GenericGSDMonitoring

import PersistedStreamEngine.Instances.EventStore.EventStoreSettings
import PersistedStreamEngine.Instances.EventStore.Read.CqrsInstance

streamWorkspaceIds :: Streamable stream monad WorkspaceId => EventStoreSettings -> stream monad (Persisted WorkspaceId)
streamWorkspaceIds settings =
    GenericGSDMonitoring.streamWorkspaceIds
      (getEventStoreStreamRepository settings)
      getEventStoreStreaming


streamCommands ::  Streamable stream monad Command => EventStoreSettings -> WorkspaceId -> stream monad (Persisted GsdCommand)
streamCommands settings workspaceId =
    GenericGSDMonitoring.streamCommands
      (getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId

streamInfinitelyCommands ::  Streamable stream monad Command => EventStoreSettings -> WorkspaceId -> stream monad (Persisted GsdCommand)
streamInfinitelyCommands settings workspaceId =
    GenericGSDMonitoring.streamInfinitelyCommands
      (getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId
