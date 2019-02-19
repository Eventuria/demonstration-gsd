{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Monitoring.Service.OverEventStore  where

import PersistedStreamEngine.Interface.Streamable

import PersistedStreamEngine.Interface.PersistedItem
import CQRS.Write.Aggregate.Commands.Command
import CQRS.Write.Aggregate.Events.Event

import Gsd.Write.Model.Commands.Command
import Gsd.Write.Model.Events.Event
import Gsd.Write.Model.Core
import Gsd.Write.Repository.EventStoreStreams
import CQRS.Write.StreamRepository
import qualified Gsd.Monitoring.Service.Generic  as GenericGSDMonitoring
import Gsd.Write.Model.State
import CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import qualified PersistedStreamEngine.Instances.EventStore.Client.Dependencies as EventStoreClient
import PersistedStreamEngine.Instances.EventStore.Read.CqrsInstance
import CQRS.Write.Aggregate.Commands.Responses.CommandResponse

import System.SafeResponse

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