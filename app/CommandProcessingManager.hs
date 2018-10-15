module CommandProcessingManager where

import Web.Scotty
import CommandSourcing.CommandHandler
import qualified CommandSourcing.CommandStream as CommandStream
import qualified CommandSourcing.WorkspaceStream as WorkspaceStream
import Data.Aeson.Encoding
import CommandSourcing.EventStore (getEventStoreConnection)
import qualified Database.EventStore as EventStore
import Control.Exception
import System.Log.Logger
import Data.UUID
import Data.Conduit
import Conduit
import qualified Control.Concurrent as Concurrent
import Control.Concurrent.Async

main :: IO ()
main = do
         let logger = "[gsd.command.processing.manager]"
         updateGlobalLogger logger $ setLevel INFO
         infoM logger "[command.processing.manager] - Starting "
         runResourceT $ do
              connection <- getEventStoreConnection
              startHandlers connection


