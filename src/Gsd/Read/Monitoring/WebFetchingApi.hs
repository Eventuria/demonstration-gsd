{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Gsd.Read.Monitoring.WebFetchingApi (execute) where


import Servant
import Network.Wai.Handler.Warp

import Prelude hiding (foldr)
import Logger.Core

import qualified Database.EventStore as EventStore
import Control.Exception hiding (Handler)


import Streamly.Prelude
import Control.Monad.IO.Class (MonadIO(..))
import PersistedStreamEngine.Instances.EventStore.EventStoreSettings
import qualified Gsd.Read.Monitoring.MonitoringOverEventStore as GsdMonitoring

import PersistedStreamEngine.Interface.PersistedItem
import Gsd.Write.Core

import Cqrs.Write.Serialization.PersistenceResult ()
import Cqrs.Write.Serialization.Command ()
import Gsd.Write.Commands

type ApiPort = Int

type GSDMonitoringFetchingApi =   FetchWorkspaceIdsCreated
                    :<|>  FetchGsdCommandsByWorkspaceId

type FetchWorkspaceIdsCreated =      "gsd" :> "monitoring" :> "fetch" :> "workspaceIds" :> Get '[JSON] [Persisted WorkspaceId]
type FetchGsdCommandsByWorkspaceId = "gsd" :> "monitoring" :> "fetch" :> "commands" :> Capture "workspaceId" WorkspaceId :> Get '[JSON] [Persisted GsdCommand]

execute :: ApiPort -> EventStore.Settings -> EventStore.ConnectionType -> EventStore.Credentials -> IO ()
execute apiPort eventStoreSettings eventStoreConnectionType credentials = do
  let logger = Logger { loggerId = "[gsd.monitoring.api]" , executableName = "monitoring.api" }
  initLogger logger

  bracket (EventStore.connect eventStoreSettings eventStoreConnectionType)
         (\connection -> do EventStore.shutdown connection
                            EventStore.waitTillClosed connection)
         (\connection -> run apiPort $ serve gsdMonitoringFetchingApi $ gsdMonitoringFetchingServer EventStoreSettings {logger, credentials, connection})


gsdMonitoringFetchingApi :: Proxy GSDMonitoringFetchingApi
gsdMonitoringFetchingApi = Proxy

gsdMonitoringFetchingServer :: EventStoreSettings  -> Server GSDMonitoringFetchingApi
gsdMonitoringFetchingServer eventStoreSettings = fetchWorkspaceIdsCreated
                                    :<|> fetchCommands
  where
        fetchWorkspaceIdsCreated :: Handler [Persisted WorkspaceId]
        fetchWorkspaceIdsCreated = do
            workspaceIds <- (liftIO $ foldr (:) [] $ GsdMonitoring.streamWorkspaceIds eventStoreSettings )
            return workspaceIds

        fetchCommands :: WorkspaceId -> Handler [Persisted GsdCommand]
        fetchCommands workspaceId = do
            commands <- liftIO $ foldr (:) []  $ GsdMonitoring.streamCommands eventStoreSettings workspaceId
            return commands

