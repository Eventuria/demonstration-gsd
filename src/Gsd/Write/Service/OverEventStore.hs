{-# LANGUAGE FlexibleContexts #-}
module Gsd.Write.Service.OverEventStore  where

import Logger.Core

import Gsd.Write.Model.Commands.Command
import Cqrs.Write.StreamRepository
import Gsd.Write.Repository.EventStoreStreams
import qualified Gsd.Write.Service.Generic as GenericGsd
import Cqrs.Write.Aggregate.Ids.AggregateId
import Cqrs.Write.Aggregate.Commands.Responses.CommandResponse
import PersistedStreamEngine.Instances.EventStore.EventStoreClientState
import PersistedStreamEngine.Instances.EventStore.TransactionInterpreter
import PersistedStreamEngine.Interface.PersistedItem
import PersistedStreamEngine.Instances.EventStore.Read.CqrsInstance
import PersistedStreamEngine.Instances.EventStore.Write.CqrsInstance
import Cqrs.Write.PersistCommandResult
import Cqrs.Write.Aggregate.Commands.CommandId
import PersistedStreamEngine.Interface.Offset
import System.SafeResponse

persistCommand ::  EventStoreClientState -> GsdCommand -> IO PersistCommandResult
persistCommand settings gsdCommand =
  GenericGsd.persistCommand
    (aggregateIdStream $ getEventStoreStreamRepository settings)
    (getCommandStream $ getEventStoreStreamRepository settings)
    getEventStoreQuerying
    getEventStoreWriting
    gsdCommand

startCommandConsumption :: EventStoreClientState -> Logger ->  IO (SafeResponse())
startCommandConsumption settings logger  =
   GenericGsd.startCommandConsumption
      logger
      (getEventStoreStreamRepository settings)
      getEventStoreReading
      (transactionInterpreterForEventStore logger (getEventStoreStreamRepository settings))


waitTillCommandResponseProduced :: EventStoreClientState ->
                              AggregateId ->
                              Offset ->
                              CommandId -> IO (SafeResponse (Persisted CommandResponse))
waitTillCommandResponseProduced settings aggregateId offset commandId =
  GenericGsd.waitTillCommandResponseProduced
    (getCommandResponseStream $ getEventStoreStreamRepository settings)
    getEventStoreSubscribing
    aggregateId
    offset
    commandId