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

module Gsd.Monitoring.WebStreamingApi (execute) where


import Servant
import Network.Wai.Handler.Warp

import Streamly.Adapters
import Servant.Pipes ()
import qualified Pipes as P

import Prelude hiding (foldr)
import Logger.Core

import qualified Database.EventStore as EventStore
import Control.Exception hiding (Handler)


import PersistedStreamEngine.Instances.EventStore.EventStoreSettings
import qualified Gsd.Monitoring.MonitoringOverEventStore as GsdMonitoring

import PersistedStreamEngine.Interface.PersistedItem
import Gsd.Write.Core

import Cqrs.Write.Serialization.PersistenceResult ()
import Cqrs.Write.Serialization.Command ()
import Gsd.Write.Commands

type ApiPort = Int

type GSDMonitoringStreamingApi =   StreamWorkspaceIdsCreated
                             :<|>  StreamGsdCommandsByWorkspaceId
                             :<|>  StreamInfinitelyGsdCommandsByWorkspaceId

type StreamWorkspaceIdsCreated =      "gsd" :> "monitoring" :> "stream" :> "workspaceIds" :> StreamGet NewlineFraming JSON (P.Producer (Persisted WorkspaceId) IO () )
type StreamGsdCommandsByWorkspaceId = "gsd" :> "monitoring" :> "stream"
                                                            :> "commands"
                                                            :> Capture "workspaceId" WorkspaceId :> StreamGet NewlineFraming JSON (P.Producer (Persisted GsdCommand) IO () )
type StreamInfinitelyGsdCommandsByWorkspaceId = "gsd" :> "monitoring" :> "stream"
                                                                      :> "infinitely"
                                                                      :> "commands" :> Capture "workspaceId" WorkspaceId :> StreamGet NewlineFraming JSON (P.Producer (Persisted GsdCommand) IO () )

execute :: ApiPort -> EventStore.Settings -> EventStore.ConnectionType -> EventStore.Credentials -> IO ()
execute apiPort eventStoreSettings eventStoreConnectionType credentials = do
  let logger = Logger { loggerId = "[gsd.monitoring.api]" , executableName = "monitoring.api" }
  initLogger logger

  bracket (EventStore.connect eventStoreSettings eventStoreConnectionType)
         (\connection -> do EventStore.shutdown connection
                            EventStore.waitTillClosed connection)
         (\connection -> run apiPort $ serve gsdMonitoringStreamingApi $ gsdMonitoringStreamingServer EventStoreSettings {logger, credentials, connection})


gsdMonitoringStreamingApi :: Proxy GSDMonitoringStreamingApi
gsdMonitoringStreamingApi = Proxy

gsdMonitoringStreamingServer :: EventStoreSettings  -> Server GSDMonitoringStreamingApi
gsdMonitoringStreamingServer eventStoreSettings = streamWorkspaceIdsCreated :<|> streamCommands :<|> streamInfinitelyCommands
  where
        streamWorkspaceIdsCreated :: Handler (P.Producer (Persisted WorkspaceId) IO ())
        streamWorkspaceIdsCreated = return $ toPipes $ GsdMonitoring.streamWorkspaceIds eventStoreSettings

        streamCommands :: WorkspaceId -> Handler (P.Producer (Persisted GsdCommand) IO ())
        streamCommands workspaceId = return $ toPipes $ GsdMonitoring.streamCommands eventStoreSettings workspaceId

        streamInfinitelyCommands :: WorkspaceId -> Handler (P.Producer (Persisted GsdCommand) IO ())
        streamInfinitelyCommands workspaceId = return $ toPipes $ GsdMonitoring.streamInfinitelyCommands eventStoreSettings workspaceId

