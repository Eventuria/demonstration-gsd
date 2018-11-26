module CommandConsumerRunner where

import Cqrs.CommandConsumerFlow
import Cqrs.Logger
import qualified Database.EventStore as EventStore
import Control.Exception
import Cqrs.Settings
import Gsd.CommandHandler
import Cqrs.Aggregate.Ids.AggregateIdStream
import Cqrs.EventStore.Context

main :: IO ()
main = do
         let logger = Logger { loggerId = "[gsd.command.processing.manager]" , executableName = "command.processing.manager" }
         initLogger logger
         logInfo logger "Starting"
         bracket (EventStore.connect getEventStoreSettings getConnectionType)
                   (\connection -> do EventStore.shutdown connection
                                      EventStore.waitTillClosed connection)
                   (\connection -> do
                      let eventStoreContext = Context {logger = logger, connection = connection , credentials = getCredentials}
                      runCommandConsumers logger eventStoreContext (getAggregateIdStream eventStoreContext)  gsdCommandHandler)



