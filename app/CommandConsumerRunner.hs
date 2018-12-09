module CommandConsumerRunner where

import Logger.Core
import qualified Database.EventStore as GYEventStore
import Control.Exception
import Settings

import Plugins.EventStore.EventStoreSettings
import qualified Gsd.GsdOverEventStore as Gsd

main :: IO ()
main = do
         let logger = Logger { loggerId = "[gsd.command.processing.manager]" , executableName = "command.processing.manager" }
         initLogger logger
         logInfo logger "Starting CommandConsumerRunner"
         bracket (GYEventStore.connect getEventStoreSettings getConnectionType)
                   (\connection -> do GYEventStore.shutdown connection
                                      GYEventStore.waitTillClosed connection)
                   (\connection ->
                      Gsd.runCommandConsumers EventStoreSettings {logger = logger, credentials = getCredentials, connection = connection} logger )



