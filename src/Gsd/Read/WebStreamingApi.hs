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

module Gsd.Read.WebStreamingApi  (execute , GSDReadStreamingApi, StreamWorkspace) where


import Servant
import Network.Wai.Handler.Warp

import Streamly.Adapters
import Servant.Pipes ()
import qualified Pipes as P

import Prelude hiding (foldr)
import Logger.Core

import qualified Database.EventStore as EventStore
import Control.Exception hiding (Handler)


import PersistedStreamEngine.Instances.EventStore.EventStoreSettings
import qualified Gsd.Read.ReadOverEventStore as GsdRead

import Gsd.Write.Core
import Gsd.Read.Goal
import PersistedStreamEngine.Interface.PersistedItem
import Gsd.Read.Workspace
import Gsd.Read.WebStreamingApiDefinition

-- Issue with Streamly, we should not need to import CQRS Event Serialization here (to investigate)
import Cqrs.Write.Serialization.Event ()


type ApiPort = Int


execute :: ApiPort -> EventStore.Settings -> EventStore.ConnectionType -> EventStore.Credentials -> IO ()
execute apiPort eventStoreSettings eventStoreConnectionType credentials = do
  let logger = Logger { loggerId = "[gsd.read.api]" , executableName = "read.api" }
  initLogger logger

  bracket (EventStore.connect eventStoreSettings eventStoreConnectionType)
         (\connection -> do EventStore.shutdown connection
                            EventStore.waitTillClosed connection)
         (\connection -> run apiPort $ serve gsdReadStreamingApi $ gsdReadStreamingServer EventStoreSettings {logger, credentials, connection})


gsdReadStreamingApi :: Proxy GSDReadStreamingApi
gsdReadStreamingApi = Proxy

gsdReadStreamingServer :: EventStoreSettings  -> Server GSDReadStreamingApi
gsdReadStreamingServer eventStoreSettings = streamWorkspace :<|> streamGoal
  where
        streamWorkspace :: Handler (P.Producer (Persisted Workspace) IO ())
        streamWorkspace = (return . toPipes) $  GsdRead.streamWorkspace eventStoreSettings

        streamGoal :: WorkspaceId -> Handler (P.Producer Goal IO ())
        streamGoal workspaceId = (return . toPipes) $ GsdRead.streamGoal eventStoreSettings workspaceId



