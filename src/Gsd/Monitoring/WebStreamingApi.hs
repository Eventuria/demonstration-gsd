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

import Gsd.Monitoring.WebStreamingApiDefinition

import PersistedStreamEngine.Instances.EventStore.EventStoreSettings
import qualified Gsd.Monitoring.MonitoringOverEventStore as GsdMonitoring

import PersistedStreamEngine.Interface.PersistedItem
import Gsd.Write.Core

import Cqrs.Write.Serialization.PersistenceResult ()
import Cqrs.Write.Serialization.Command ()
import Cqrs.Write.Serialization.Event ()
import Gsd.Write.Commands.Command
import Gsd.Write.Commands.Serialization ()
import Gsd.Write.Events.Event
import Gsd.Write.Events.Serialization()
import Gsd.Write.State
import Cqrs.Write.Aggregate.Commands.ValidationStates.ValidationState
import Cqrs.Write.Serialization.ValidationState ()
import Cqrs.Write.Aggregate.Commands.Responses.CommandResponse
import Cqrs.Write.Serialization.CommandResponse ()

type ApiPort = Int


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
gsdMonitoringStreamingServer eventStoreSettings = streamWorkspaceId
                                             :<|> streamCommand
                                             :<|> streamInfinitelyCommand
                                             :<|> streamCommandResponse
                                             :<|> streamEvent
                                             :<|> streamInfinitelyEvent
                                             :<|> streamGsdValidationStateByWorkspaceId
  where
        streamWorkspaceId :: Handler (P.Producer (Persisted WorkspaceId) IO ())
        streamWorkspaceId = return $ toPipes $ GsdMonitoring.streamWorkspaceId eventStoreSettings

        streamCommandResponse :: WorkspaceId -> Handler (P.Producer (Persisted CommandResponse) IO ())
        streamCommandResponse workspaceId = return $ toPipes $ GsdMonitoring.streamCommandResponse eventStoreSettings workspaceId

        streamInfinitelyCommand :: WorkspaceId -> Handler (P.Producer (Persisted GsdCommand) IO ())
        streamInfinitelyCommand workspaceId = return $ toPipes $ GsdMonitoring.streamInfinitelyCommand eventStoreSettings workspaceId

        streamCommand :: WorkspaceId -> Handler (P.Producer (Persisted GsdCommand) IO ())
        streamCommand workspaceId = return $ toPipes $ GsdMonitoring.streamCommand eventStoreSettings workspaceId

        streamEvent :: WorkspaceId -> Handler (P.Producer (Persisted GsdEvent) IO ())
        streamEvent workspaceId = return $ toPipes $ GsdMonitoring.streamEvent eventStoreSettings workspaceId

        streamInfinitelyEvent :: WorkspaceId -> Handler (P.Producer (Persisted GsdEvent) IO ())
        streamInfinitelyEvent workspaceId = return $ toPipes $ GsdMonitoring.streamInfinitelyEvent eventStoreSettings workspaceId

        streamGsdValidationStateByWorkspaceId :: WorkspaceId -> Handler (P.Producer (Persisted (ValidationState GsdState)) IO ())
        streamGsdValidationStateByWorkspaceId workspaceId = return $ toPipes $ GsdMonitoring.streamValidationState eventStoreSettings workspaceId