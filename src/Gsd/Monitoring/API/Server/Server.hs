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
{-# LANGUAGE FlexibleContexts #-}

module Gsd.Monitoring.API.Server.Server  where


import Servant

import Streamly.Adapters
import Servant.Pipes ()
import qualified Pipes as P
import Prelude hiding (foldr)
import Network.Wai.Handler.Warp
import Gsd.Monitoring.API.Definition

import PersistedStreamEngine.Instances.EventStore.EventStoreClientManager
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
import DevOps.Core
import System.SafeResponse
import Gsd.Monitoring.API.Server.Settings
import Logger.Core

start :: ServerSettings -> IO ()
start ServerSettings {port,eventStoreClientSettings,logger} = do

    logInfo logger "Starting Server"

    bracketEventStoreClientManager
      eventStoreClientSettings
      (\eventStoreClientManager -> run port (application eventStoreClientManager))

  where
    application :: EventStoreClientManager  -> Application
    application eventStoreClientManager = serve proxy $ server eventStoreClientManager

    proxy :: Proxy GSDMonitoringStreamingApi
    proxy = Proxy

    server :: EventStoreClientManager  -> Server GSDMonitoringStreamingApi
    server eventStoreSettings @ EventStoreClientManager {logger} =
        healthCheck
         :<|> streamCommand
         :<|> streamInfinitelyCommand
         :<|> streamCommandResponse
         :<|> streamEvent
         :<|> streamInfinitelyEvent
         :<|> streamGsdValidationStateByWorkspaceId
      where

        healthCheck :: Handler HealthCheckResult
        healthCheck = return Healthy

        streamCommandResponse :: WorkspaceId -> Handler (P.Producer (SafeResponse (Persisted CommandResponse)) IO ())
        streamCommandResponse workspaceId = return $ toPipes $ GsdMonitoring.streamCommandResponse
                                                                  eventStoreSettings
                                                                  workspaceId

        streamInfinitelyCommand :: WorkspaceId -> Handler (P.Producer (SafeResponse (Persisted GsdCommand)) IO ())
        streamInfinitelyCommand workspaceId = return $ toPipes $ GsdMonitoring.streamInfinitelyCommand
                                                                    eventStoreSettings
                                                                    workspaceId

        streamCommand :: WorkspaceId -> Handler (P.Producer (SafeResponse (Persisted GsdCommand)) IO ())
        streamCommand workspaceId =
            return $ toPipes $ GsdMonitoring.streamCommand eventStoreSettings workspaceId


        streamEvent :: WorkspaceId -> Handler (P.Producer (SafeResponse (Persisted GsdEvent)) IO ())
        streamEvent workspaceId = return $ toPipes $ GsdMonitoring.streamEvent eventStoreSettings workspaceId

        streamInfinitelyEvent :: WorkspaceId -> Handler (P.Producer (SafeResponse (Persisted GsdEvent)) IO ())
        streamInfinitelyEvent workspaceId = return $ toPipes $ GsdMonitoring.streamInfinitelyEvent
                                                                  eventStoreSettings
                                                                  workspaceId

        streamGsdValidationStateByWorkspaceId :: WorkspaceId ->
                                                 Handler (P.Producer (SafeResponse (Persisted (ValidationState GsdState))) IO ())
        streamGsdValidationStateByWorkspaceId workspaceId = return $ toPipes $ GsdMonitoring.streamValidationState
                                                                                  eventStoreSettings
                                                                                  workspaceId

