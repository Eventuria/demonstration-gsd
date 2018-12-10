{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module WriteApi where

import Settings

import Web.Scotty
import Prelude hiding (foldr)
import Logger.Core

import qualified Database.EventStore as EventStore
import Control.Exception

import Control.Monad.IO.Class (MonadIO(..))
import Plugins.EventStore.EventStoreSettings
import qualified Gsd.Write.GsdOverEventStore as Gsd.Write

import Cqrs.Write.Serialization.PersistenceResult ()


main :: IO ()
main = do
  let logger = Logger { loggerId = "[gsd.write.api]" , executableName = "write.api" }
  initLogger logger
  logInfo logger "[write.api] - Starting"

  bracket (EventStore.connect getEventStoreSettings getConnectionType)
         (\connection -> do EventStore.shutdown connection
                            EventStore.waitTillClosed connection)
         (\connection -> routing logger $ EventStoreSettings {logger = logger, credentials = getCredentials, connection = connection})


routing :: Logger -> EventStoreSettings  -> IO()
routing logger eventStoreSettings = scotty getWriteApiPort $ do
  post "/sendCommand/" $ do
     liftIO $ logInfo logger "post /sendCommand/"
     command <- jsonData
     commands <- (liftIO $ Gsd.Write.persistCommand eventStoreSettings command )
     json commands


