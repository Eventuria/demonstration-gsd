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
import Network.Wai.Handler.Warp

import Streamly.Adapters
import Servant.Pipes ()
import qualified Pipes as P

import Prelude hiding (foldr)
import Logger.Core
import Control.Monad.IO.Class (MonadIO(liftIO))

import PersistedStreamEngine.Instances.EventStore.EventStoreClientManager
import qualified Gsd.Read.ReadOverEventStore as GsdRead

import Gsd.Write.Core
import Gsd.Read.Goal
import Gsd.Read.Action
import PersistedStreamEngine.Interface.PersistedItem
import Gsd.Read.Workspace
import Gsd.Read.API.Definition
import System.SafeResponse
import Gsd.Read.API.Server.ServerSettings

start :: ServerSettings -> IO ()
start ServerSettings {port,eventStoreClientSettings,logger}    = do

    logInfo logger "Starting Server"

    bracketEventStoreClientManager
      eventStoreClientSettings
      (\eventStoreClientManager -> run port (application eventStoreClientManager))

    where
      application :: EventStoreClientManager  -> Application
      application eventStoreClientManager = serve proxy $ server eventStoreClientManager

      proxy :: Proxy GSDReadApi
      proxy = Proxy

      server :: EventStoreClientManager  -> Server GSDReadApi
      server eventStoreClientManager = streamWorkspace
                                              :<|> streamGoal
                                              :<|> streamAction
                                              :<|> fetchWorkspace
                                              :<|> fetchGoal
       where
        streamWorkspace :: Handler (P.Producer (SafeResponse (Persisted Workspace)) IO ())
        streamWorkspace = (return . toPipes) $  GsdRead.streamWorkspace eventStoreClientManager

        fetchWorkspace :: WorkspaceId -> Handler (SafeResponse (Maybe Workspace))
        fetchWorkspace workspaceId = (liftIO $ GsdRead.fetchWorkspace eventStoreClientManager workspaceId)

        streamGoal :: WorkspaceId -> Handler (P.Producer (SafeResponse Goal) IO ())
        streamGoal workspaceId = (return . toPipes) $ GsdRead.streamGoal eventStoreClientManager workspaceId

        fetchGoal :: WorkspaceId -> GoalId -> Handler (SafeResponse (Maybe Goal))
        fetchGoal workspaceId goalId = (liftIO $ GsdRead.fetchGoal eventStoreClientManager workspaceId goalId)

        streamAction :: WorkspaceId -> GoalId -> Handler (P.Producer (SafeResponse Action) IO ())
        streamAction workspaceId goalId = (return . toPipes) $ GsdRead.streamAction eventStoreClientManager workspaceId goalId



