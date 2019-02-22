{-# LANGUAGE FlexibleContexts #-}
module Eventuria.GSD.Write.Flow.Sourcer.Service.OverEventStore  where

import Eventuria.GSD.Write.Model.Commands.Command
import Eventuria.Libraries.CQRS.Write.StreamRepository
import Eventuria.GSD.Write.Repository.EventStoreStreams
import qualified Eventuria.GSD.Write.Flow.Sourcer.Service.Generic as GenericGsd
import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import qualified Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Dependencies as EventStoreClient
import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Read.CqrsInstance
import Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Write.CqrsInstance
import Eventuria.Libraries.CQRS.Write.PersistCommandResult
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import Eventuria.Commons.System.SafeResponse

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