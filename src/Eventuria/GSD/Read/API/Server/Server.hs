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

module Eventuria.GSD.Read.API.Server.Server  (start) where


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
                 
import           Eventuria.Commons.Logger.Core
import           Eventuria.Commons.Dependencies.Core
import           Eventuria.Commons.Network.Core
import           Eventuria.Commons.System.Threading
import           Eventuria.Commons.Dependencies.HealthChecking
                 
import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem

import qualified Eventuria.GSD.Read.Service.OverEventStore as Service
import qualified Eventuria.GSD.Read.API.Server.Dependencies as Server
import           Eventuria.GSD.Read.Model.Goal
import           Eventuria.GSD.Read.Model.Action
import           Eventuria.GSD.Read.Model.Workspace
import           Eventuria.GSD.Read.API.Definition
import           Eventuria.GSD.Read.API.Server.Settings
                 
import           Eventuria.GSD.Write.Model.Core


start :: Settings -> IO ()
start settings @ Settings {healthCheckLoggerId} = do
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
    runServerOnWarp dependencies @ Server.Dependencies {port,logger}  = do
      logInfo logger "Server Up and Running"
      serverThreadId <- myThreadId
      serverDownExceptionReceiver <- catchingServerDownExceptionOnceAndThenDiscard serverThreadId
      run port $ application
                (proxy :: Proxy GSDReadApi)
                (readServer serverDownExceptionReceiver)
                dependencies

    {-- N.B : Servant does not support Streamly,
              so Streamly is converted to Pipe at the Servant Level (see toPipes )
    --}
    readServer :: ServerThreadId -> ServantServer GSDReadApi Server.Dependencies
    readServer serverThreadId dependencies =
      healthCheck            serverThreadId dependencies
        :<|> streamWorkspace serverThreadId dependencies
        :<|> streamGoal      serverThreadId dependencies
        :<|> streamAction    serverThreadId dependencies
        :<|> fetchWorkspace  serverThreadId dependencies
        :<|> fetchGoal       serverThreadId dependencies
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

      streamWorkspace :: ServerThreadId ->
                         Server.Dependencies ->
                         Handler (PipeStream ( Persisted Workspace))
      streamWorkspace serverThreadId Server.Dependencies {logger, eventStoreClientDependencies} = do
        liftIO $ logInfo logger "stream workspaces"
        return . toPipes $ Service.streamWorkspace eventStoreClientDependencies
                              & Streamly.mapM (breakServerOnFailure logger serverThreadId)

      fetchWorkspace :: ServerThreadId ->
                        Server.Dependencies ->
                        WorkspaceId ->
                        Handler (Maybe Workspace)
      fetchWorkspace serverThreadId Server.Dependencies {logger,eventStoreClientDependencies} workspaceId = do
        liftIO $ logInfo logger "fetch workspace"
        liftIO $ Service.fetchWorkspace eventStoreClientDependencies workspaceId >>=
                 breakServerOnFailure logger serverThreadId

      streamGoal :: ServerThreadId ->
                    Server.Dependencies ->
                    WorkspaceId ->
                    Handler (PipeStream Goal)
      streamGoal serverThreadId Server.Dependencies {logger,eventStoreClientDependencies} workspaceId = do
        liftIO $ logInfo logger "stream goals"
        (return . toPipes) $ Service.streamGoal eventStoreClientDependencies workspaceId
                                & Streamly.mapM (breakServerOnFailure logger serverThreadId)

      fetchGoal :: ServerThreadId ->
                   Server.Dependencies ->
                   WorkspaceId ->
                   GoalId ->
                   Handler (Maybe Goal)
      fetchGoal serverThreadId Server.Dependencies {logger,eventStoreClientDependencies} workspaceId goalId = do
        liftIO $ logInfo logger "fetch goal"
        liftIO $ Service.fetchGoal
                    eventStoreClientDependencies
                    workspaceId
                    goalId >>=
                 breakServerOnFailure logger serverThreadId

      streamAction :: ServerThreadId ->
                      Server.Dependencies ->
                      WorkspaceId ->
                      GoalId ->
                      Handler (PipeStream Action )
      streamAction serverThreadId Server.Dependencies {logger,eventStoreClientDependencies} workspaceId goalId = do
        liftIO $ logInfo logger "stream action"
        (return . toPipes) $ Service.streamAction
                              eventStoreClientDependencies
                              workspaceId
                              goalId
                           & Streamly.mapM (breakServerOnFailure logger serverThreadId)


