{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Gsd.Write.Api where


import Web.Scotty
import Prelude hiding (foldr)
import Logger.Core

import qualified Database.EventStore as EventStore
import Control.Exception

import Control.Monad.IO.Class (MonadIO(..))
import PersistedStreamEngine.Instances.EventStore.EventStoreSettings
import qualified Gsd.Write.GsdOverEventStore as Gsd.Write

import Cqrs.Write.Serialization.PersistenceResult ()

type ApiPort = Int

execute ::  ApiPort -> EventStore.Settings -> EventStore.ConnectionType -> EventStore.Credentials -> IO ()
execute apiPort eventStoreSettings eventStoreConnectionType credentials = do
  let logger = Logger { loggerId = "[gsd.write.api]" , executableName = "write.api" }
  initLogger logger
  logInfo logger "[write.api] - Starting"

  bracket (EventStore.connect eventStoreSettings eventStoreConnectionType )
         (\connection -> do EventStore.shutdown connection
                            EventStore.waitTillClosed connection)
         (\connection -> routing logger apiPort $ EventStoreSettings {logger, credentials, connection})


routing :: Logger -> ApiPort -> EventStoreSettings  -> IO()
routing logger apiPort eventStoreSettings = scotty apiPort $ do
  post "/sendCommand/" $ do
     liftIO $ logInfo logger "post /sendCommand/"
     command <- jsonData
     commands <- (liftIO $ Gsd.Write.persistCommand eventStoreSettings command )
     json commands


