module Gsd.Write.GsdOverEventStore (persistCommand,streamCommandConsumption) where

import Logger.Core

import PersistedStreamEngine.Interface.Write.PersistenceResult


import Gsd.Write.Commands.Command
import Cqrs.Write.StreamRepository
import Gsd.Write.EventStoreStreamRepository
import qualified Gsd.Write.GenericGsd as GenericGsd

import PersistedStreamEngine.Instances.EventStore.EventStoreSettings
import PersistedStreamEngine.Instances.EventStore.CqrsEDSLInterpreter

import PersistedStreamEngine.Instances.EventStore.Read.CqrsInstance
import PersistedStreamEngine.Instances.EventStore.Write.CqrsInstance


persistCommand ::  EventStoreSettings -> GsdCommand -> IO PersistenceResult
persistCommand settings gsdCommand =
  GenericGsd.persistCommand
    (aggregateIdStream $ getEventStoreStreamRepository settings)
    (getCommandStream $ getEventStoreStreamRepository settings)
    getEventStoreQuerying
    getEventStoreWriting
    gsdCommand

streamCommandConsumption :: EventStoreSettings -> Logger ->  IO ()
streamCommandConsumption settings logger  =
   GenericGsd.streamCommandConsumption
      (getEventStoreStreamRepository settings)
      getEventStoreReading
      interpretWriteEventStoreLanguage
      logger

