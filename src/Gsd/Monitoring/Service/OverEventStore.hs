{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Gsd.Monitoring.Service.OverEventStore  where

import PersistedStreamEngine.Interface.Streamable

import PersistedStreamEngine.Interface.PersistedItem
import Cqrs.Write.Aggregate.Commands.Command
import Cqrs.Write.Aggregate.Events.Event

import Gsd.Write.Model.Commands.Command
import Gsd.Write.Model.Events.Event
import Gsd.Write.Model.Core
import Gsd.Write.Repository.EventStoreStreams
import Cqrs.Write.StreamRepository
import qualified Gsd.Monitoring.Service.Generic  as GenericGSDMonitoring
import Gsd.Write.Model.State
import Cqrs.Write.Aggregate.Commands.ValidationStates.ValidationState
import PersistedStreamEngine.Instances.EventStore.EventStoreClientState
import PersistedStreamEngine.Instances.EventStore.Read.CqrsInstance
import Cqrs.Write.Aggregate.Commands.Responses.CommandResponse
--import DevOps.Core (HealthCheckResult)
--import DevOps.MicroService.EventStore
import System.SafeResponse

--healthCheck :: EventStoreMicroService -> IO HealthCheckResult
--healthCheck eventStoreMicroService = healthCheck eventStoreMicroService

streamWorkspaceId :: Streamable stream monad WorkspaceId =>
                      EventStoreClientState ->
                      stream monad (SafeResponse (Persisted WorkspaceId))
streamWorkspaceId settings =
    GenericGSDMonitoring.streamWorkspaceId
      (aggregateIdStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming


streamCommand :: Streamable stream monad Command =>
                  EventStoreClientState ->
                  WorkspaceId ->
                  stream monad (SafeResponse (Persisted GsdCommand))
streamCommand settings workspaceId =
   GenericGSDMonitoring.streamCommand
     (getCommandStream $ getEventStoreStreamRepository settings)
     getEventStoreStreaming
     workspaceId



streamInfinitelyCommand :: Streamable stream monad Command =>
                            EventStoreClientState ->
                            WorkspaceId ->
                            stream monad (SafeResponse (Persisted GsdCommand))
streamInfinitelyCommand settings workspaceId =
    GenericGSDMonitoring.streamInfinitelyCommand
      (getCommandStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId

streamCommandResponse :: Streamable stream monad CommandResponse =>
                          EventStoreClientState ->
                          WorkspaceId ->
                          stream monad (SafeResponse (Persisted CommandResponse))
streamCommandResponse settings workspaceId =
    GenericGSDMonitoring.streamCommandResponse
      (getCommandResponseStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId

streamEvent :: Streamable stream monad Event =>
                EventStoreClientState ->
                WorkspaceId ->
                stream monad (SafeResponse (Persisted GsdEvent))
streamEvent settings workspaceId =
    GenericGSDMonitoring.streamEvent
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId

streamInfinitelyEvent :: Streamable stream monad Event =>
                          EventStoreClientState ->
                          WorkspaceId ->
                          stream monad (SafeResponse (Persisted GsdEvent))
streamInfinitelyEvent settings workspaceId =
    GenericGSDMonitoring.streamInfinitelyEvent
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId

streamValidationState :: Streamable stream monad (ValidationState GsdState) =>
                          EventStoreClientState ->
                          WorkspaceId ->
                          stream monad (SafeResponse (Persisted (ValidationState GsdState)))
streamValidationState settings workspaceId =
    GenericGSDMonitoring.streamValidationState
      (getValidationStateStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId