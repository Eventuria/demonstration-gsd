module Gsd.Write.GsdOverEventStore (persistCommand,streamCommandConsumption) where

import Logger.Core

import PersistedStreamEngine.Write.PersistenceResult


import Gsd.Write.Commands

import Gsd.Write.EventStoreStreamRepository
import qualified Gsd.Write.GenericGsd as GenericGsd

import Plugins.EventStore.EventStoreSettings
import Plugins.EventStore.CqrsEDSLInterpreter

import Plugins.EventStore.Read.CqrsInstance
import Plugins.EventStore.Write.CqrsInstance


persistCommand ::  EventStoreSettings -> GsdCommand -> IO PersistenceResult
persistCommand settings gsdCommand =
  GenericGsd.persistCommand
    (getEventStoreStreamRepository settings)
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

