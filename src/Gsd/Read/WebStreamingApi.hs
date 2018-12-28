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

module Gsd.Read.WebStreamingApi  (execute , GSDReadStreamingApi, StreamWorkspaces) where


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

import PersistedStreamEngine.Interface.PersistedItem
import Gsd.Read.Workspace


type ApiPort = Int

type GSDReadStreamingApi =   StreamWorkspaces

type StreamWorkspaces =   "gsd" :> "read" :> "stream" :> "workspaces" :> StreamGet NewlineFraming JSON (P.Producer (Persisted Workspace) IO () )

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
gsdReadStreamingServer eventStoreSettings = streamWorkspaces
  where
        streamWorkspaces :: Handler (P.Producer (Persisted Workspace) IO ())
        streamWorkspaces = return $ toPipes $ GsdRead.streamWorkspaces eventStoreSettings





