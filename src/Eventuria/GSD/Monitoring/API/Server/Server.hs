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

import           Prelude hiding (foldr)

import           Data.Function ((&))

import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Concurrent
import           Control.Exception hiding (Handler)

import           Servant
import           Servant.Pipes ()
import           Network.Wai.Handler.Warp hiding (Settings)

import qualified Streamly.Prelude as Streamly

import           Eventuria.Adapters.Servant.Wrapper
import           Eventuria.Adapters.Streamly.Adapters

import           Eventuria.Commons.Dependencies.Core
import           Eventuria.Commons.Logger.Core
import           Eventuria.Commons.Dependencies.HealthChecking
import           Eventuria.Commons.Network.Core
import           Eventuria.Commons.System.Threading

import qualified Eventuria.GSD.Monitoring.Service.OverEventStore as GsdMonitoring

import           Eventuria.Libraries.CQRS.Write.Serialization.PersistenceResult ()
import           Eventuria.Libraries.CQRS.Write.Serialization.Command ()
import           Eventuria.Libraries.CQRS.Write.Serialization.Event ()

import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import           Eventuria.Libraries.CQRS.Write.Serialization.CommandResponse ()
                 
import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
                 
import           Eventuria.GSD.Write.Model.Core
import           Eventuria.GSD.Write.Model.Commands.Command
import           Eventuria.GSD.Write.Model.Commands.Serialization ()
import           Eventuria.GSD.Write.Model.Events.Event
import           Eventuria.GSD.Write.Model.Events.Serialization()
import           Eventuria.GSD.Write.Model.WriteModel
                 
import           Eventuria.GSD.Monitoring.API.Definition
import           Eventuria.GSD.Monitoring.API.Server.Settings
import qualified Eventuria.GSD.Monitoring.API.Server.Dependencies as Server.State
import qualified Eventuria.GSD.Monitoring.API.Server.Dependencies as Server


start :: Settings -> IO ()
start settings @ Settings {healthCheckLoggerId}  = do
  waitTillHealthy
      healthCheckLoggerId
      settings
      Server.getDependencies
      Server.healthCheck
  catch
    (Server.getDependencies
       settings
       (runServerOnWarp))
    (\ServerDownException -> start settings)


  where

    runServerOnWarp :: Server.Dependencies -> IO()
    runServerOnWarp dependencies @ Server.Dependencies {logger,port} = do
       logInfo logger "Server Up and Running"
       serverThreadId <- myThreadId
       serverDownExceptionReceiver <- catchingServerDownExceptionOnceAndThenDiscard serverThreadId
       run port $ application
                    (proxy :: Proxy GSDMonitoringStreamingApi)
                    (monitoringServer serverDownExceptionReceiver)
                    dependencies

    {-- N.B : Servant does not support Streamly,
              so Streamly is converted to Pipe at the Servant Level (see toPipes )
    --}
    monitoringServer :: ServerThreadId -> ServantServer GSDMonitoringStreamingApi Server.Dependencies
    monitoringServer serverThreadId dependencies =
              healthCheck                             serverThreadId dependencies
         :<|> streamCommand                           serverThreadId dependencies
         :<|> streamCommandResponse                   serverThreadId dependencies
         :<|> streamEvent                             serverThreadId dependencies
         :<|> streamGsdWriteModelHistoryByWorkspaceId serverThreadId dependencies
      where

        healthCheck :: ServerThreadId -> Server.Dependencies -> Handler Healthy
        healthCheck serverThreadId Server.Dependencies {logger} =
          liftIO $ logInfo logger "service health asked"  >>
                   Server.healthCheck dependencies >>=
                   either
                     (\error -> do
                         logInfo logger $ "service unhealthy : " ++ show error
                         return $ Left $ toException ServerDownException)
                     (\right -> do
                         logInfo logger "service healthy"
                         return $ Right ()) >>=
                   breakServerOnFailure logger serverThreadId

        streamCommandResponse :: ServerThreadId ->
                                 Server.Dependencies ->
                                 WorkspaceId ->
                                 Handler (PipeStream (Persisted CommandResponse))
        streamCommandResponse serverThreadId
                              Server.Dependencies {logger,eventStoreClientDependencies}
                              workspaceId =
          return . toPipes $ GsdMonitoring.streamCommandResponse eventStoreClientDependencies workspaceId
                           & Streamly.mapM (breakServerOnFailure logger serverThreadId)

        streamCommand :: ServerThreadId ->
                         Server.Dependencies ->
                         WorkspaceId ->
                         Handler (PipeStream (Persisted GsdCommand))
        streamCommand serverThreadId
                      Server.Dependencies {logger,eventStoreClientDependencies}
                      workspaceId =
          return . toPipes $  GsdMonitoring.streamCommand eventStoreClientDependencies workspaceId
                           & Streamly.mapM (breakServerOnFailure logger serverThreadId)

        streamEvent :: ServerThreadId ->
                       Server.Dependencies ->
                       WorkspaceId ->
                       Handler (PipeStream (Persisted GsdEvent))
        streamEvent serverThreadId
                    Server.Dependencies {logger,eventStoreClientDependencies}
                    workspaceId =
          return . toPipes $  GsdMonitoring.streamEvent eventStoreClientDependencies workspaceId
                           & Streamly.mapM (breakServerOnFailure logger serverThreadId)


        streamGsdWriteModelHistoryByWorkspaceId :: ServerThreadId ->
                                                 Server.Dependencies ->
                                                 WorkspaceId ->
                                                 Handler (PipeStream (Persisted (Maybe GsdWriteModel)))
        streamGsdWriteModelHistoryByWorkspaceId serverThreadId
                                              Server.Dependencies {logger,eventStoreClientDependencies}
                                              workspaceId =
          return . toPipes $ GsdMonitoring.streamWriteModelHistory eventStoreClientDependencies workspaceId
                           & Streamly.mapM (breakServerOnFailure logger serverThreadId)


