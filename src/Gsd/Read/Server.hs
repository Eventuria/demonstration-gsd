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

module Gsd.Read.Server  (execute , GSDReadApi, StreamWorkspace) where


import Servant
import Network.Wai.Handler.Warp

import Streamly.Adapters
import Servant.Pipes ()
import qualified Pipes as P

import Prelude hiding (foldr)
import Logger.Core
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Database.EventStore as EventStore
import Control.Exception hiding (Handler)


import PersistedStreamEngine.Instances.EventStore.EventStoreSettings
import qualified Gsd.Read.ReadOverEventStore as GsdRead

import Gsd.Write.Core
import Gsd.Read.Goal
import Gsd.Read.Action
import PersistedStreamEngine.Interface.PersistedItem
import Gsd.Read.Workspace
import Gsd.Read.WebApiDefinition
import System.SafeResponse

type ApiPort = Int

execute :: ApiPort ->
           EventStore.Settings ->
           EventStore.ConnectionType ->
           EventStore.Credentials ->
           IO ()
execute apiPort eventStoreSettings eventStoreConnectionType credentials = do
  let logger = Logger { loggerId = "[gsd.read.api]" , executableName = "read.api" }
  initLogger logger

  bracket (EventStore.connect eventStoreSettings eventStoreConnectionType)
         (\connection -> do EventStore.shutdown connection
                            EventStore.waitTillClosed connection)
         (\connection -> run
                          apiPort $
                          serve gsdReadApi $
                          gsdReadStreamingServer EventStoreSettings {logger, credentials, connection})


gsdReadApi :: Proxy GSDReadApi
gsdReadApi = Proxy

gsdReadStreamingServer :: EventStoreSettings  -> Server GSDReadApi
gsdReadStreamingServer eventStoreSettings = streamWorkspace
                                        :<|> streamGoal
                                        :<|> streamAction
                                        :<|> fetchWorkspace
                                        :<|> fetchGoal
  where
    streamWorkspace :: Handler (P.Producer (SafeResponse (Persisted Workspace)) IO ())
    streamWorkspace = (return . toPipes) $  GsdRead.streamWorkspace eventStoreSettings

    fetchWorkspace :: WorkspaceId -> Handler (SafeResponse (Maybe Workspace))
    fetchWorkspace workspaceId = (liftIO $ GsdRead.fetchWorkspace eventStoreSettings workspaceId)

    streamGoal :: WorkspaceId -> Handler (P.Producer (SafeResponse Goal) IO ())
    streamGoal workspaceId = (return . toPipes) $ GsdRead.streamGoal eventStoreSettings workspaceId

    fetchGoal :: WorkspaceId -> GoalId -> Handler (SafeResponse (Maybe Goal))
    fetchGoal workspaceId goalId = (liftIO $ GsdRead.fetchGoal eventStoreSettings workspaceId goalId)

    streamAction :: WorkspaceId -> GoalId -> Handler (P.Producer (SafeResponse Action) IO ())
    streamAction workspaceId goalId = (return . toPipes) $ GsdRead.streamAction eventStoreSettings workspaceId goalId



