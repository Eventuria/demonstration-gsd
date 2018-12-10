module CommandConsumptionStreamer where

import Logger.Core
import qualified Database.EventStore as GYEventStore
import Control.Exception
import Settings

import Plugins.EventStore.EventStoreSettings
import qualified Gsd.Write.GsdOverEventStore as Gsd.Write

main :: IO ()
main = do
  let logger = Logger { loggerId = "[gsd.command.processing.manager]" , executableName = "command.processing.manager" }
  initLogger logger

  logInfo logger "Starting Command Consumption Streamer"
  bracket (GYEventStore.connect getEventStoreSettings getConnectionType)
           (\connection -> do GYEventStore.shutdown connection
                              GYEventStore.waitTillClosed connection)
           (\connection ->do
              let eventStoreSettings = EventStoreSettings {logger = logger, credentials = getCredentials, connection = connection}
              Gsd.Write.streamCommandConsumption eventStoreSettings logger )



