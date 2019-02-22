{-# LANGUAGE FlexibleContexts #-}
module Gsd.Write.Flow.CommandConsumer.Service.OverEventStore  where

import Eventuria.Commons.Logger.Core

import Gsd.Write.Repository.EventStoreStreams
import qualified Gsd.Write.Flow.CommandConsumer.Service.Generic as GenericGsd
import qualified PersistedStreamEngine.Instances.EventStore.Client.Dependencies as EventStoreClient
import PersistedStreamEngine.Instances.EventStore.TransactionInterpreter
import PersistedStreamEngine.Instances.EventStore.Read.CqrsInstance
import Eventuria.Commons.System.SafeResponse

consumeCommands :: Logger -> EventStoreClient.Dependencies -> IO (SafeResponse())
consumeCommands logger eventStoreClientDependencies =
   GenericGsd.consumeCommands
      logger
      (getEventStoreStreamRepository eventStoreClientDependencies)
      getEventStoreReading
      (transactionInterpreterForEventStore logger (getEventStoreStreamRepository eventStoreClientDependencies))

