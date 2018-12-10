{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module GsdMonitoringApi where

import Settings

import Web.Scotty
import Prelude hiding (foldr)
import Logger.Core

import qualified Database.EventStore as EventStore
import Control.Exception
import Data.UUID

import Streamly.Prelude
import Control.Monad.IO.Class (MonadIO(..))
import PersistedStreamEngine.Instances.EventStore.EventStoreSettings
import qualified Gsd.Read.Monitoring.MonitoringOverEventStore as GsdMonitoring

import Cqrs.Write.Serialization.PersistenceResult ()
import Cqrs.Write.Serialization.Command ()
import Gsd.Write.Commands ()

main :: IO ()
main = do
  let logger = Logger { loggerId = "[gsd.monitoring.api]" , executableName = "monitoring.api" }
  initLogger logger
  logInfo logger "[monitoring.api] - Starting"

  bracket (EventStore.connect getEventStoreSettings getConnectionType)
         (\connection -> do EventStore.shutdown connection
                            EventStore.waitTillClosed connection)
         (\connection -> routing logger $ EventStoreSettings {logger = logger, credentials = getCredentials, connection = connection})


routing :: Logger -> EventStoreSettings  -> IO()
routing logger eventStoreSettings = scotty getGsdMonitoringApiPort $ do
  get  "/readWorkspaceIds" $ do
    workspaceIds <- (liftIO $ foldr (:) [] $ GsdMonitoring.streamWorkspaceIds eventStoreSettings )
    liftIO $ logInfo logger $ "[api] - result > " ++ show workspaceIds
    json workspaceIds
  get  "/readCommands/:workspaceIdGiven" $ do
    workspaceIdString <- param "workspaceIdGiven"
    let workspaceIdOpt = fromString workspaceIdString
    case workspaceIdOpt of
                   Just (workspaceId) -> (liftIO $ foldr (:) []  $ GsdMonitoring.streamCommands eventStoreSettings workspaceId) >>= json
                   Nothing -> html "you've passed an invalid workspace id format"


