{-# LANGUAGE FlexibleContexts #-}
module Gsd.Monitoring.MonitoringOverEventStore (
                streamWorkspaceIds,
                streamCommands,
                streamInfinitelyCommands,
                streamEvents,
                streamInfinitelyEvents) where

import PersistedStreamEngine.Interface.Streamable

import PersistedStreamEngine.Interface.PersistedItem
import Cqrs.Write.Aggregate.Commands.Command
import Cqrs.Write.Aggregate.Events.Event

import Gsd.Write.Commands.Command
import Gsd.Write.Events.Event
import Gsd.Write.Core
import Gsd.Write.EventStoreStreamRepository

import qualified Gsd.Monitoring.GenericMonitoring  as GenericGSDMonitoring

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

streamEvents ::  Streamable stream monad Event => EventStoreSettings -> WorkspaceId -> stream monad (Persisted GsdEvent)
streamEvents settings workspaceId =
    GenericGSDMonitoring.streamEvents
      (getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId

streamInfinitelyEvents ::  Streamable stream monad Event => EventStoreSettings -> WorkspaceId -> stream monad (Persisted GsdEvent)
streamInfinitelyEvents settings workspaceId =
    GenericGSDMonitoring.streamInfinitelyEvents
      (getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId