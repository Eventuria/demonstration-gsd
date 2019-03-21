{-# LANGUAGE FlexibleContexts #-}
module Eventuria.GSD.Write.CommandSourcer.Service.OverEventStore  where


import           Control.Exception

import qualified Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Dependencies as EventStoreClient
import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import           Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Read.CqrsInstance
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import           Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Write.CqrsInstance

import           Eventuria.Libraries.CQRS.Write.PersistCommandResult
import           Eventuria.Libraries.CQRS.Write.StreamRepository
import           Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId

import           Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandTransaction
import qualified Eventuria.GSD.Write.CommandSourcer.Service.Generic as GSD.Service.Generic
import           Eventuria.GSD.Write.Model.Commands.Command
import           Eventuria.GSD.Write.Repository.EventStoreStreams


persistCommand ::  EventStoreClient.Dependencies -> GsdCommand -> IO (Either SomeException PersistCommandResult)
persistCommand eventStoreClientDependencies gsdCommand =
  GSD.Service.Generic.persistCommand
    (aggregateIdStream $ getEventStoreStreamRepository eventStoreClientDependencies)
    (getCommandStream $ getEventStoreStreamRepository eventStoreClientDependencies)
    getEventStoreQuerying
    getEventStoreWriting
    gsdCommand

waitTillCommandResponseProduced :: EventStoreClient.Dependencies ->
                                   AggregateId ->
                                   Offset ->
                                   CommandId ->
                                   IO (Either SomeException (Persisted CommandTransaction))
waitTillCommandResponseProduced eventStoreClientDependencies aggregateId offset commandId =
  GSD.Service.Generic.waitTillCommandResponseProduced
    (getCommandTransactionStream $ getEventStoreStreamRepository eventStoreClientDependencies)
    getEventStoreSubscribing
    aggregateId
    offset
    commandId