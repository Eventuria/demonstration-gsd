{-# LANGUAGE FlexibleContexts #-}
module Eventuria.GSD.Write.Flow.CommandConsumer.Service.OverEventStore  where

import Eventuria.Commons.Logger.Core

import Eventuria.GSD.Write.Repository.EventStoreStreams
import qualified Eventuria.GSD.Write.Flow.CommandConsumer.Service.Generic as GenericGsd
import qualified Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Dependencies as EventStoreClient
import Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.TransactionInterpreter
import Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Read.CqrsInstance
import Eventuria.Commons.System.SafeResponse

consumeCommands :: Logger -> EventStoreClient.Dependencies -> IO (SafeResponse())
consumeCommands logger eventStoreClientDependencies =
   GenericGsd.consumeCommands
      logger
      (getEventStoreStreamRepository eventStoreClientDependencies)
      getEventStoreReading
      (transactionInterpreterForEventStore logger (getEventStoreStreamRepository eventStoreClientDependencies))

