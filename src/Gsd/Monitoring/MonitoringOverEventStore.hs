{-# LANGUAGE FlexibleContexts #-}
module Gsd.Monitoring.MonitoringOverEventStore (
                streamWorkspaceId,
                streamCommand,
                streamInfinitelyCommand,
                streamCommandResponse,
                streamEvent,
                streamInfinitelyEvent,
                streamValidationState) where

import PersistedStreamEngine.Interface.Streamable

import PersistedStreamEngine.Interface.PersistedItem
import Cqrs.Write.Aggregate.Commands.Command
import Cqrs.Write.Aggregate.Events.Event

import Gsd.Write.Commands.Command
import Gsd.Write.Events.Event
import Gsd.Write.Core
import Gsd.Write.EventStoreStreamRepository
import Cqrs.Write.StreamRepository
import qualified Gsd.Monitoring.GenericMonitoring  as GenericGSDMonitoring
import Gsd.Write.State
import Cqrs.Write.Aggregate.Commands.ValidationStates.ValidationState
import PersistedStreamEngine.Instances.EventStore.EventStoreSettings
import PersistedStreamEngine.Instances.EventStore.Read.CqrsInstance
import Cqrs.Write.Aggregate.Commands.Responses.CommandResponse

streamWorkspaceId :: Streamable stream monad WorkspaceId => EventStoreSettings -> stream monad (Persisted WorkspaceId)
streamWorkspaceId settings =
    GenericGSDMonitoring.streamWorkspaceId
      (aggregateIdStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming


streamCommand ::  Streamable stream monad Command => EventStoreSettings -> WorkspaceId -> stream monad (Persisted GsdCommand)
streamCommand settings workspaceId =
    GenericGSDMonitoring.streamCommand
      (getCommandStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId

streamInfinitelyCommand ::  Streamable stream monad Command => EventStoreSettings -> WorkspaceId -> stream monad (Persisted GsdCommand)
streamInfinitelyCommand settings workspaceId =
    GenericGSDMonitoring.streamInfinitelyCommand
      (getCommandStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId

streamCommandResponse ::  Streamable stream monad CommandResponse => EventStoreSettings -> WorkspaceId -> stream monad (Persisted CommandResponse)
streamCommandResponse settings workspaceId =
    GenericGSDMonitoring.streamCommandResponse
      (getCommandResponseStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId

streamEvent ::  Streamable stream monad Event => EventStoreSettings -> WorkspaceId -> stream monad (Persisted GsdEvent)
streamEvent settings workspaceId =
    GenericGSDMonitoring.streamEvent
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId

streamInfinitelyEvent ::  Streamable stream monad Event => EventStoreSettings -> WorkspaceId -> stream monad (Persisted GsdEvent)
streamInfinitelyEvent settings workspaceId =
    GenericGSDMonitoring.streamInfinitelyEvent
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId

streamValidationState ::  Streamable stream monad (ValidationState GsdState) => EventStoreSettings -> WorkspaceId -> stream monad (Persisted (ValidationState GsdState))
streamValidationState settings workspaceId =
    GenericGSDMonitoring.streamValidationState
      (getValidationStateStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId