{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.GSD.Monitoring.Service.OverEventStore  where

import           Control.Exception

import           Streamly hiding (Streaming)

import qualified Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Dependencies as EventStoreClient
import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import           Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Read.CqrsInstance

import           Eventuria.Libraries.CQRS.Write.StreamRepository
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import           Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Read.ReadProjections

import           Eventuria.GSD.Write.CommandConsumer.Handling.ProjectGSDWriteModel
import           Eventuria.GSD.Write.Model.Commands.Command
import           Eventuria.GSD.Write.Model.Events.Event
import           Eventuria.GSD.Write.Model.Core
import           Eventuria.GSD.Write.Model.WriteModel
import           Eventuria.GSD.Write.Repository.EventStoreStreams
import           Eventuria.Libraries.CQRS.Write.Serialization.Command ()
import qualified Eventuria.GSD.Monitoring.Service.Generic  as GenericGSDMonitoring



streamWorkspaceId :: EventStoreClient.Dependencies ->
                     SerialT IO (Either SomeException (Persisted WorkspaceId))
streamWorkspaceId eventStoreClientDependencies =
    GenericGSDMonitoring.streamWorkspaceId
      (streamAllAggregateId (aggregateIdStream $ getEventStoreStreamRepository eventStoreClientDependencies))


streamCommand :: EventStoreClient.Dependencies ->
                 WorkspaceId ->
                 SerialT IO (Either SomeException (Persisted GSDCommand))
streamCommand eventStoreClientDependencies workspaceId =
   GenericGSDMonitoring.streamCommand
     (getCommandStream $ getEventStoreStreamRepository eventStoreClientDependencies)
     getEventStoreStreaming
     workspaceId

streamCommandResponse :: EventStoreClient.Dependencies ->
                         WorkspaceId ->
                         SerialT IO (Either SomeException (Persisted CommandResponse))
streamCommandResponse eventStoreClientDependencies workspaceId =
    GenericGSDMonitoring.streamCommandResponse
      (getStreamAllCommandResponseByAggregateId (getCommandTransactionStream $ getEventStoreStreamRepository eventStoreClientDependencies))
      workspaceId

streamEvent :: EventStoreClient.Dependencies ->
               WorkspaceId ->
               SerialT IO (Either SomeException (Persisted GsdEvent))
streamEvent eventStoreClientDependencies workspaceId =
    GenericGSDMonitoring.streamEvent
      (getStreamAllEventsByAggregateId (getCommandTransactionStream $ getEventStoreStreamRepository eventStoreClientDependencies))
      workspaceId


streamWriteModelHistory :: EventStoreClient.Dependencies ->
                           WorkspaceId ->
                           SerialT IO (Either SomeException (Persisted (Maybe GsdWriteModel)))
streamWriteModelHistory eventStoreClientDependencies workspaceId =
    GenericGSDMonitoring.streamWriteModelHistory
      (getStreamAllWriteModelByAggregateId
          (getCommandTransactionStream $ getEventStoreStreamRepository eventStoreClientDependencies)
          projectGSDWriteModel)
      workspaceId