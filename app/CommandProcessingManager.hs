module CommandProcessingManager where

import Web.Scotty
import Cqrs.CommandProcessingStarter
import Cqrs.Logger
import qualified Database.EventStore as EventStore
import Control.Exception
import Cqrs.EventStore
import Data.UUID
import qualified Control.Concurrent as Concurrent
import Control.Concurrent.Async
import Gsd.CommandHandler


main :: IO ()
main = do
         let logger = Logger { loggerId = "[gsd.command.processing.manager]" , executableName = "command.processing.manager" }
         initLogger logger
         logInfo logger "Starting"
         bracket (EventStore.connect EventStore.defaultSettings (EventStore.Static "127.0.0.1" 1113))
                   (\connection -> do EventStore.shutdown connection
                                      EventStore.waitTillClosed connection)
                   (\connection -> startProcessingCommands logger getCredentials connection gsdCommandHandler)



