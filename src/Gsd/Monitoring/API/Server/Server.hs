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
import Network.Wai.Handler.Warp hiding (Settings)
import Gsd.Monitoring.API.Definition

import PersistedStreamEngine.Instances.EventStore.EventStoreClientState
import qualified Gsd.Monitoring.Service.OverEventStore as GsdMonitoring

import PersistedStreamEngine.Interface.PersistedItem
import Gsd.Write.Model.Core

import CQRS.Write.Serialization.PersistenceResult ()
import CQRS.Write.Serialization.Command ()
import CQRS.Write.Serialization.Event ()
import Gsd.Write.Model.Commands.Command
import Gsd.Write.Model.Commands.Serialization ()
import Gsd.Write.Model.Events.Event
import Gsd.Write.Model.Events.Serialization()
import Gsd.Write.Model.State
import CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import CQRS.Write.Serialization.ValidationState ()
import CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import CQRS.Write.Serialization.CommandResponse ()
import DevOps.Core
import System.SafeResponse
import Gsd.Monitoring.API.Server.Settings
import qualified Gsd.Monitoring.API.Server.State as Server.State
import Gsd.Monitoring.API.Server.State
import Logger.Core


start :: Settings -> IO ()
start settings   = do

  Server.State.getState
    settings
    (\State {port, logger, eventStoreClientState } -> do
        logInfo logger "Starting Server"
        run port (application eventStoreClientState))

  where
    application :: EventStoreClientState  -> Application
    application eventStoreClientState = serve proxy $ server eventStoreClientState

    proxy :: Proxy GSDMonitoringStreamingApi
    proxy = Proxy

    server :: EventStoreClientState  -> Server GSDMonitoringStreamingApi
    server eventStoreSettings @ EventStoreClientState {logger} =
        healthCheck
         :<|> streamCommand
         :<|> streamInfinitelyCommand
         :<|> streamCommandResponse
         :<|> streamEvent
         :<|> streamInfinitelyEvent
         :<|> streamGsdValidationStateByWorkspaceId
      where

        healthCheck :: Handler HealthCheckResult
        healthCheck = return healthy

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

