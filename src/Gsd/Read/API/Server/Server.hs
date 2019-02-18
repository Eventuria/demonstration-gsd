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
import Network.Wai.Handler.Warp hiding (Settings)

import Streamly.Adapters
import Servant.Pipes ()
import qualified Pipes as P

import Prelude hiding (foldr)
import Logger.Core
import Control.Monad.IO.Class (MonadIO(liftIO))

import PersistedStreamEngine.Instances.EventStore.EventStoreClientState
import qualified Gsd.Read.Service.OverEventStore as GsdRead

import Gsd.Write.Model.Core
import Gsd.Read.Model.Goal
import Gsd.Read.Model.Action
import PersistedStreamEngine.Interface.PersistedItem
import Gsd.Read.Model.Workspace
import Gsd.Read.API.Definition
import System.SafeResponse
import Gsd.Read.API.Server.Settings
import qualified Gsd.Read.API.Server.State as Server.State
import Gsd.Read.API.Server.State
import DevOps.Core

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

    proxy :: Proxy GSDReadApi
    proxy = Proxy

    server :: EventStoreClientState  -> Server GSDReadApi
    server eventStoreClientState = healthCheck
                                            :<|> streamWorkspace
                                            :<|> streamGoal
                                            :<|> streamAction
                                            :<|> fetchWorkspace
                                            :<|> fetchGoal
     where
      healthCheck :: Handler HealthCheckResult
      healthCheck = return healthy

      streamWorkspace :: Handler (P.Producer (SafeResponse (Persisted Workspace)) IO ())
      streamWorkspace = (return . toPipes) $  GsdRead.streamWorkspace
                                                        eventStoreClientState

      fetchWorkspace :: WorkspaceId -> Handler (SafeResponse (Maybe Workspace))
      fetchWorkspace workspaceId = (liftIO $ GsdRead.fetchWorkspace
                                                        eventStoreClientState
                                                        workspaceId)

      streamGoal :: WorkspaceId -> Handler (P.Producer (SafeResponse Goal) IO ())
      streamGoal workspaceId = (return . toPipes) $ GsdRead.streamGoal
                                                              eventStoreClientState
                                                              workspaceId

      fetchGoal :: WorkspaceId -> GoalId -> Handler (SafeResponse (Maybe Goal))
      fetchGoal workspaceId goalId = (liftIO $ GsdRead.fetchGoal
                                                          eventStoreClientState
                                                          workspaceId
                                                          goalId)

      streamAction :: WorkspaceId -> GoalId -> Handler (P.Producer (SafeResponse Action) IO ())
      streamAction workspaceId goalId = (return . toPipes) $ GsdRead.streamAction
                                                                        eventStoreClientState
                                                                        workspaceId
                                                                        goalId



