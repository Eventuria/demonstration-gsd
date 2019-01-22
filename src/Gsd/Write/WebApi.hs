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

import Servant.Pipes ()

import Control.Monad.IO.Class (MonadIO(..))
import PersistedStreamEngine.Instances.EventStore.EventStoreSettings
import qualified Gsd.Write.GsdOverEventStore as Gsd.Write
import Cqrs.Write.Aggregate.Commands.Responses.CommandResponse
import Cqrs.Write.Aggregate.Ids.AggregateId

import Cqrs.Write.Serialization.PersistenceResult ()
import Gsd.Write.WebApiDefinition
import Servant
import Network.Wai.Handler.Warp
import Gsd.Write.Commands.Command
import Gsd.Write.Commands.Serialization ()
import Cqrs.Write.PersistCommandResult
import PersistedStreamEngine.Interface.PersistedItem
import Cqrs.Write.Aggregate.Commands.CommandId
import PersistedStreamEngine.Interface.Offset
type ApiPort = Int

execute ::  ApiPort -> EventStore.Settings -> EventStore.ConnectionType -> EventStore.Credentials -> IO ()
execute apiPort eventStoreSettings eventStoreConnectionType credentials = do
  let logger = Logger { loggerId = "[gsd.write.api]" , executableName = "write.api" }
  initLogger logger
  logInfo logger "[write.api] - Starting"

  bracket (EventStore.connect eventStoreSettings eventStoreConnectionType )
         (\connection -> do EventStore.shutdown connection
                            EventStore.waitTillClosed connection)
         (\connection -> run
                          apiPort $
                          serve gsdWriteApi $
                          gsdWriteServer $
                          EventStoreSettings {logger, credentials, connection})

gsdWriteApi :: Proxy GsdWriteApi
gsdWriteApi = Proxy

gsdWriteServer :: EventStoreSettings  -> Server GsdWriteApi
gsdWriteServer eventStoreSettings = sendGsdCommand :<|> waitTillCommandResponseProduced
  where
        sendGsdCommand :: GsdCommand -> Handler PersistCommandResult
        sendGsdCommand gsdCommand = (liftIO $ Gsd.Write.persistCommand eventStoreSettings gsdCommand )


        waitTillCommandResponseProduced :: AggregateId -> Offset -> CommandId -> Handler (Persisted CommandResponse)
        waitTillCommandResponseProduced aggregateId offset commandId =
          liftIO $ Gsd.Write.waitTillCommandResponseProduced eventStoreSettings aggregateId offset commandId

