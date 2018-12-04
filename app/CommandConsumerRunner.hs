module CommandConsumerRunner where

import Logger.Core
import qualified Database.EventStore as GYEventStore
import Control.Exception
import Cqrs.Settings
import Plugins.GregYoungEventStore.Settings
import qualified Gsd.Gsd as Gsd
import qualified Plugins.GregYoungEventStore.Instance as EventStore
main :: IO ()
main = do
         let logger = Logger { loggerId = "[gsd.command.processing.manager]" , executableName = "command.processing.manager" }
         initLogger logger
         logInfo logger "Starting CommandConsumerRunner"
         bracket (GYEventStore.connect getEventStoreSettings getConnectionType)
                   (\connection -> do GYEventStore.shutdown connection
                                      GYEventStore.waitTillClosed connection)
                   (\connection ->
                      Gsd.runCommandConsumers logger Context {logger = logger, connection = connection , credentials = getCredentials} EventStore.getEventStoreReading )



