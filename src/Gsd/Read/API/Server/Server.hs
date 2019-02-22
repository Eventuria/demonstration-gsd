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

module Gsd.Read.API.Server.Server  (start) where


import Servant
import Eventuria.Adapters.Servant.Wrapper
import Servant.Pipes ()
import Network.Wai.Handler.Warp hiding (Settings)
import Eventuria.Adapters.Streamly.Adapters


import Prelude hiding (foldr)
import Eventuria.Commons.Logger.Core
import Control.Monad.IO.Class (MonadIO(liftIO))

import qualified Gsd.Read.Service.OverEventStore as GsdRead

import Gsd.Write.Model.Core
import Gsd.Read.Model.Goal
import Gsd.Read.Model.Action
import PersistedStreamEngine.Interface.PersistedItem
import Gsd.Read.Model.Workspace
import Gsd.Read.API.Definition
import Eventuria.Commons.System.SafeResponse
import Gsd.Read.API.Server.Settings
import qualified Gsd.Read.API.Server.Dependencies as Server
import Eventuria.Commons.DevOps.Core
import Eventuria.Commons.Dependencies.RetrieveByHealthChecking


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
                    (proxy :: Proxy GSDReadApi)
                    readServer
                    dependencies

    {-- N.B : Servant does not support Streamly,
              so Streamly is converted to Pipe at the Servant Level (see toPipes )
    --}
    readServer :: ServantServer GSDReadApi Server.Dependencies
    readServer dependencies = healthCheck
                                :<|> streamWorkspace dependencies
                                :<|> streamGoal      dependencies
                                :<|> streamAction    dependencies
                                :<|> fetchWorkspace  dependencies
                                :<|> fetchGoal       dependencies
     where
      healthCheck :: Handler HealthCheckResult
      healthCheck = return healthy

      streamWorkspace :: Server.Dependencies ->
                         Handler (PipeStream (SafeResponse (Persisted Workspace)))
      streamWorkspace Server.Dependencies {eventStoreClientDependencies} =
        (return . toPipes . GsdRead.streamWorkspace) eventStoreClientDependencies

      fetchWorkspace :: Server.Dependencies ->
                        WorkspaceId ->
                        Handler (SafeResponse (Maybe Workspace))
      fetchWorkspace Server.Dependencies {eventStoreClientDependencies} workspaceId =
        liftIO $ GsdRead.fetchWorkspace eventStoreClientDependencies workspaceId

      streamGoal :: Server.Dependencies ->
                    WorkspaceId ->
                    Handler (PipeStream (SafeResponse Goal))
      streamGoal Server.Dependencies {eventStoreClientDependencies} workspaceId =
        (return . toPipes) $ GsdRead.streamGoal eventStoreClientDependencies workspaceId


      fetchGoal :: Server.Dependencies ->
                   WorkspaceId ->
                   GoalId ->
                   Handler (SafeResponse (Maybe Goal))
      fetchGoal Server.Dependencies {eventStoreClientDependencies} workspaceId goalId =
        liftIO $ GsdRead.fetchGoal
                    eventStoreClientDependencies
                    workspaceId
                    goalId

      streamAction :: Server.Dependencies ->
                      WorkspaceId ->
                      GoalId ->
                      Handler (PipeStream (SafeResponse Action) )
      streamAction Server.Dependencies {eventStoreClientDependencies} workspaceId goalId =
        (return . toPipes) $ GsdRead.streamAction
                              eventStoreClientDependencies
                              workspaceId
                              goalId



