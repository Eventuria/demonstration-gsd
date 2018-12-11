module Gsd.Write.CommandConsumptionStreamer where

import Logger.Core
import qualified Database.EventStore as EventStore
import Control.Exception


import PersistedStreamEngine.Instances.EventStore.EventStoreSettings
import qualified Gsd.Write.GsdOverEventStore as Gsd.Write

execute :: EventStore.Settings -> EventStore.ConnectionType -> EventStore.Credentials -> IO ()
execute eventStoreSettings eventStoreConnectionType eventStoreCredentials = do
  let logger = Logger { loggerId = "[gsd.command.processing.manager]" , executableName = "command.processing.manager" }
  initLogger logger

  logInfo logger "Starting Command Consumption Streamer"
  bracket (EventStore.connect eventStoreSettings eventStoreConnectionType)
           (\connection -> do EventStore.shutdown connection
                              EventStore.waitTillClosed connection)
           (\connection ->do
              let eventStoreSettings = EventStoreSettings {logger = logger, credentials = eventStoreCredentials, connection = connection}
              Gsd.Write.streamCommandConsumption eventStoreSettings logger )



