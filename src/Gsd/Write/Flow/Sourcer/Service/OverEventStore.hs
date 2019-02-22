{-# LANGUAGE FlexibleContexts #-}
module Gsd.Write.Flow.Sourcer.Service.OverEventStore  where

import Gsd.Write.Model.Commands.Command
import CQRS.Write.StreamRepository
import Gsd.Write.Repository.EventStoreStreams
import qualified Gsd.Write.Flow.Sourcer.Service.Generic as GenericGsd
import CQRS.Write.Aggregate.Ids.AggregateId
import CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import qualified PersistedStreamEngine.Instances.EventStore.Client.Dependencies as EventStoreClient
import PersistedStreamEngine.Interface.PersistedItem
import PersistedStreamEngine.Instances.EventStore.Read.CqrsInstance
import PersistedStreamEngine.Instances.EventStore.Write.CqrsInstance
import CQRS.Write.PersistCommandResult
import CQRS.Write.Aggregate.Commands.CommandId
import PersistedStreamEngine.Interface.Offset
import System.SafeResponse

persistCommand ::  EventStoreClient.Dependencies -> GsdCommand -> IO PersistCommandResult
persistCommand eventStoreClientDependencies gsdCommand =
  GenericGsd.persistCommand
    (aggregateIdStream $ getEventStoreStreamRepository eventStoreClientDependencies)
    (getCommandStream $ getEventStoreStreamRepository eventStoreClientDependencies)
    getEventStoreQuerying
    getEventStoreWriting
    gsdCommand

waitTillCommandResponseProduced :: EventStoreClient.Dependencies ->
                                   AggregateId ->
                                   Offset ->
                                   CommandId ->
                                   IO (SafeResponse (Persisted CommandResponse))
waitTillCommandResponseProduced eventStoreClientDependencies aggregateId offset commandId =
  GenericGsd.waitTillCommandResponseProduced
    (getCommandResponseStream $ getEventStoreStreamRepository eventStoreClientDependencies)
    getEventStoreSubscribing
    aggregateId
    offset
    commandId