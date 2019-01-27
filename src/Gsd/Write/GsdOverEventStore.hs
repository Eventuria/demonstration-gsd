{-# LANGUAGE FlexibleContexts #-}
module Gsd.Write.GsdOverEventStore  where

import Logger.Core

import Gsd.Write.Commands.Command
import Cqrs.Write.StreamRepository
import Gsd.Write.EventStoreStreamRepository
import qualified Gsd.Write.GenericGsd as GenericGsd
import Cqrs.Write.Aggregate.Ids.AggregateId
import Cqrs.Write.Aggregate.Commands.Responses.CommandResponse
import PersistedStreamEngine.Instances.EventStore.EventStoreSettings
import PersistedStreamEngine.Instances.EventStore.TransactionInterpreter
import PersistedStreamEngine.Interface.PersistedItem
import PersistedStreamEngine.Instances.EventStore.Read.CqrsInstance
import PersistedStreamEngine.Instances.EventStore.Write.CqrsInstance
import Cqrs.Write.PersistCommandResult
import Cqrs.Write.Aggregate.Commands.CommandId
import PersistedStreamEngine.Interface.Offset
import System.SafeResponse

persistCommand ::  EventStoreSettings -> GsdCommand -> IO PersistCommandResult
persistCommand settings gsdCommand =
  GenericGsd.persistCommand
    (aggregateIdStream $ getEventStoreStreamRepository settings)
    (getCommandStream $ getEventStoreStreamRepository settings)
    getEventStoreQuerying
    getEventStoreWriting
    gsdCommand

startCommandConsumption :: EventStoreSettings -> Logger ->  IO (SafeResponse())
startCommandConsumption settings logger  =
   GenericGsd.startCommandConsumption
      logger
      (getEventStoreStreamRepository settings)
      getEventStoreReading
      (transactionInterpreterForEventStore logger (getEventStoreStreamRepository settings))


waitTillCommandResponseProduced :: EventStoreSettings ->
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