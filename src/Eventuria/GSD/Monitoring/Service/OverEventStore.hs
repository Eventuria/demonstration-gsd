{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.GSD.Monitoring.Service.OverEventStore  where

import Eventuria.Libraries.PersistedStreamEngine.Interface.Streamable

import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import Eventuria.Libraries.CQRS.Write.Aggregate.Events.Event

import Eventuria.GSD.Write.Model.Commands.Command
import Eventuria.GSD.Write.Model.Events.Event
import Eventuria.GSD.Write.Model.Core
import Eventuria.GSD.Write.Repository.EventStoreStreams
import Eventuria.Libraries.CQRS.Write.StreamRepository
import qualified Eventuria.GSD.Monitoring.Service.Generic  as GenericGSDMonitoring
import Eventuria.GSD.Write.Model.State
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import qualified Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Dependencies as EventStoreClient
import Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Read.CqrsInstance
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse

import Eventuria.Commons.System.SafeResponse

streamWorkspaceId :: Streamable stream monad WorkspaceId =>
                      EventStoreClient.Dependencies ->
                      stream monad (SafeResponse (Persisted WorkspaceId))
streamWorkspaceId eventStoreClientDependencies =
    GenericGSDMonitoring.streamWorkspaceId
      (aggregateIdStream $ getEventStoreStreamRepository eventStoreClientDependencies)
      getEventStoreStreaming


streamCommand :: Streamable stream monad Command =>
                  EventStoreClient.Dependencies ->
                  WorkspaceId ->
                  stream monad (SafeResponse (Persisted GsdCommand))
streamCommand eventStoreClientDependencies workspaceId =
   GenericGSDMonitoring.streamCommand
     (getCommandStream $ getEventStoreStreamRepository eventStoreClientDependencies)
     getEventStoreStreaming
     workspaceId



streamInfinitelyCommand :: Streamable stream monad Command =>
                            EventStoreClient.Dependencies ->
                            WorkspaceId ->
                            stream monad (SafeResponse (Persisted GsdCommand))
streamInfinitelyCommand eventStoreClientDependencies workspaceId =
    GenericGSDMonitoring.streamInfinitelyCommand
      (getCommandStream $ getEventStoreStreamRepository eventStoreClientDependencies)
      getEventStoreStreaming
      workspaceId

streamCommandResponse :: Streamable stream monad CommandResponse =>
                          EventStoreClient.Dependencies ->
                          WorkspaceId ->
                          stream monad (SafeResponse (Persisted CommandResponse))
streamCommandResponse eventStoreClientDependencies workspaceId =
    GenericGSDMonitoring.streamCommandResponse
      (getCommandResponseStream $ getEventStoreStreamRepository eventStoreClientDependencies)
      getEventStoreStreaming
      workspaceId

streamEvent :: Streamable stream monad Event =>
                EventStoreClient.Dependencies ->
                WorkspaceId ->
                stream monad (SafeResponse (Persisted GsdEvent))
streamEvent eventStoreClientDependencies workspaceId =
    GenericGSDMonitoring.streamEvent
      (getEventStream $ getEventStoreStreamRepository eventStoreClientDependencies)
      getEventStoreStreaming
      workspaceId

streamInfinitelyEvent :: Streamable stream monad Event =>
                          EventStoreClient.Dependencies ->
                          WorkspaceId ->
                          stream monad (SafeResponse (Persisted GsdEvent))
streamInfinitelyEvent eventStoreClientDependencies workspaceId =
    GenericGSDMonitoring.streamInfinitelyEvent
      (getEventStream $ getEventStoreStreamRepository eventStoreClientDependencies)
      getEventStoreStreaming
      workspaceId

streamValidationState :: Streamable stream monad (ValidationState GsdState) =>
                          EventStoreClient.Dependencies ->
                          WorkspaceId ->
                          stream monad (SafeResponse (Persisted (ValidationState GsdState)))
streamValidationState eventStoreClientDependencies workspaceId =
    GenericGSDMonitoring.streamValidationState
      (getValidationStateStream $ getEventStoreStreamRepository eventStoreClientDependencies)
      getEventStoreStreaming
      workspaceId