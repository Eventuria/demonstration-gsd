module CommandConsumerRunner where

import Logger.Core
import qualified Database.EventStore as EventStore
import Control.Exception
import Cqrs.Settings
import EventStore.Settings
import qualified Gsd.Gsd as Gsd

main :: IO ()
main = do
         let logger = Logger { loggerId = "[gsd.command.processing.manager]" , executableName = "command.processing.manager" }
         initLogger logger
         logInfo logger "Starting CommandConsumerRunner"
         bracket (EventStore.connect getEventStoreSettings getConnectionType)
                   (\connection -> do EventStore.shutdown connection
                                      EventStore.waitTillClosed connection)
                   (\connection ->
                      Gsd.runCommandConsumers logger Context {logger = logger, connection = connection , credentials = getCredentials} )



