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

module Eventuria.GSD.Monitoring.API.Server.Server  where

import Prelude hiding (foldr)

import Servant
import Eventuria.Adapters.Servant.Wrapper
import Servant.Pipes ()
import Eventuria.Adapters.Streamly.Adapters
import Network.Wai.Handler.Warp hiding (Settings)
import Eventuria.GSD.Monitoring.API.Definition

import qualified Eventuria.GSD.Monitoring.Service.OverEventStore as GsdMonitoring

import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import Eventuria.GSD.Write.Model.Core

import Eventuria.Libraries.CQRS.Write.Serialization.PersistenceResult ()
import Eventuria.Libraries.CQRS.Write.Serialization.Command ()
import Eventuria.Libraries.CQRS.Write.Serialization.Event ()
import Eventuria.GSD.Write.Model.Commands.Command
import Eventuria.GSD.Write.Model.Commands.Serialization ()
import Eventuria.GSD.Write.Model.Events.Event
import Eventuria.GSD.Write.Model.Events.Serialization()
import Eventuria.GSD.Write.Model.State
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import Eventuria.Libraries.CQRS.Write.Serialization.ValidationState ()
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import Eventuria.Libraries.CQRS.Write.Serialization.CommandResponse ()
import Eventuria.Commons.DevOps.Core
import Eventuria.GSD.Monitoring.API.Server.Settings
import qualified Eventuria.GSD.Monitoring.API.Server.Dependencies as Server.State
import qualified Eventuria.GSD.Monitoring.API.Server.Dependencies as Server
import Eventuria.Commons.Logger.Core
import Eventuria.Commons.Dependencies.RetrieveByHealthChecking
import Eventuria.Commons.System.SafeResponse

start :: Settings -> IO ()
start settings @ Settings {healthCheckLoggerId}  =
  checkHealthAndRetrieveDependencies
    healthCheckLoggerId
    settings
    Server.retrieveDependencies
    runServerOnWarp

  where

    runServerOnWarp :: Server.Dependencies -> IO()
    runServerOnWarp dependencies @ Server.Dependencies {logger,port} = do
       logInfo logger "Server Started"
       run port $ application
                    (proxy :: Proxy GSDMonitoringStreamingApi)
                    monitoringServer
                    dependencies

    {-- N.B : Servant does not support Streamly,
              so Streamly is converted to Pipe at the Servant Level (see toPipes )
    --}
    monitoringServer :: ServantServer GSDMonitoringStreamingApi Server.Dependencies
    monitoringServer dependencies = healthCheck
         :<|> streamCommand                         dependencies
         :<|> streamInfinitelyCommand               dependencies
         :<|> streamCommandResponse                 dependencies
         :<|> streamEvent                           dependencies
         :<|> streamInfinitelyEvent                 dependencies
         :<|> streamGsdValidationStateByWorkspaceId dependencies
      where

        healthCheck :: Handler HealthCheckResult
        healthCheck = return healthy

        streamCommandResponse :: Server.Dependencies ->
                                 WorkspaceId ->
                                 Handler (PipeStream (SafeResponse (Persisted CommandResponse)))
        streamCommandResponse Server.Dependencies {eventStoreClientDependencies} =
          return . toPipes . GsdMonitoring.streamCommandResponse eventStoreClientDependencies


        streamInfinitelyCommand :: Server.Dependencies ->
                                   WorkspaceId ->
                                   Handler (PipeStream (SafeResponse (Persisted GsdCommand)))
        streamInfinitelyCommand Server.Dependencies {eventStoreClientDependencies} =
          return . toPipes . GsdMonitoring.streamInfinitelyCommand eventStoreClientDependencies


        streamCommand :: Server.Dependencies ->
                         WorkspaceId ->
                         Handler (PipeStream (SafeResponse (Persisted GsdCommand)))
        streamCommand Server.Dependencies {eventStoreClientDependencies} =
          return . toPipes .  GsdMonitoring.streamCommand eventStoreClientDependencies

        streamEvent :: Server.Dependencies ->
                       WorkspaceId ->
                       Handler (PipeStream (SafeResponse (Persisted GsdEvent)))
        streamEvent Server.Dependencies {eventStoreClientDependencies} =
          return . toPipes .  GsdMonitoring.streamEvent eventStoreClientDependencies

        streamInfinitelyEvent :: Server.Dependencies ->
                                 WorkspaceId ->
                                 Handler (PipeStream (SafeResponse (Persisted GsdEvent)))
        streamInfinitelyEvent Server.Dependencies {eventStoreClientDependencies} =
          return . toPipes . GsdMonitoring.streamInfinitelyEvent eventStoreClientDependencies

        streamGsdValidationStateByWorkspaceId :: Server.Dependencies ->
                                                 WorkspaceId ->
                                                 Handler (PipeStream (SafeResponse (Persisted (ValidationState GsdState))))
        streamGsdValidationStateByWorkspaceId Server.Dependencies {eventStoreClientDependencies} =
          return . toPipes . GsdMonitoring.streamValidationState eventStoreClientDependencies


