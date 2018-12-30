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
module Gsd.Write.WebApi (execute,GsdWriteApi) where

import Prelude hiding (foldr)
import Logger.Core

import qualified Database.EventStore as EventStore
import Control.Exception hiding (Handler)

import Control.Monad.IO.Class (MonadIO(..))
import PersistedStreamEngine.Instances.EventStore.EventStoreSettings
import qualified Gsd.Write.GsdOverEventStore as Gsd.Write

import PersistedStreamEngine.Interface.Write.PersistenceResult
import Cqrs.Write.Serialization.PersistenceResult ()

import Servant
import Network.Wai.Handler.Warp
import Gsd.Write.Commands.Command
import Gsd.Write.Commands.Serialization ()

type ApiPort = Int

type GsdWriteApi =   SendGsdCommand

type SendGsdCommand = "gsd" :> "write" :> "sendCommand" :> ReqBody '[JSON] GsdCommand :> PostAccepted '[JSON] PersistenceResult


execute ::  ApiPort -> EventStore.Settings -> EventStore.ConnectionType -> EventStore.Credentials -> IO ()
execute apiPort eventStoreSettings eventStoreConnectionType credentials = do
  let logger = Logger { loggerId = "[gsd.write.api]" , executableName = "write.api" }
  initLogger logger
  logInfo logger "[write.api] - Starting"

  bracket (EventStore.connect eventStoreSettings eventStoreConnectionType )
         (\connection -> do EventStore.shutdown connection
                            EventStore.waitTillClosed connection)
         (\connection -> run apiPort $ serve gsdWriteApi $ gsdWriteServer $ EventStoreSettings {logger, credentials, connection})

gsdWriteApi :: Proxy GsdWriteApi
gsdWriteApi = Proxy

gsdWriteServer :: EventStoreSettings  -> Server GsdWriteApi
gsdWriteServer eventStoreSettings = sendGsdCommand
  where
        sendGsdCommand :: GsdCommand -> Handler PersistenceResult
        sendGsdCommand gsdCommand = do
            result <- (liftIO $ Gsd.Write.persistCommand eventStoreSettings gsdCommand )
            return result

