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
module Gsd.Write.API.Server.Server (start) where

import Prelude hiding (foldr)
import Logger.Core
import Servant.Pipes ()

import Control.Monad.IO.Class (MonadIO(..))
import PersistedStreamEngine.Instances.EventStore.EventStoreClientManager
import qualified Gsd.Write.GsdOverEventStore as Gsd.Write
import Cqrs.Write.Aggregate.Commands.Responses.CommandResponse
import Cqrs.Write.Aggregate.Ids.AggregateId

import Cqrs.Write.Serialization.PersistenceResult ()
import Gsd.Write.API.Definition
import Servant
import Network.Wai.Handler.Warp
import Gsd.Write.Commands.Command
import Gsd.Write.Commands.Serialization ()
import Cqrs.Write.PersistCommandResult
import PersistedStreamEngine.Interface.PersistedItem
import Cqrs.Write.Aggregate.Commands.CommandId
import PersistedStreamEngine.Interface.Offset
import System.SafeResponse
import Gsd.Write.API.Server.Settings


start ::  ServerSettings -> IO ()
start ServerSettings {port,eventStoreClientSettings,logger}    = do
  logInfo logger "Starting Write Server"

  bracketEventStoreClientManager
    eventStoreClientSettings
    (\eventStoreClientManager -> run port (application eventStoreClientManager))

  where
    application :: EventStoreClientManager  -> Application
    application eventStoreClientManager = serve proxy $ server eventStoreClientManager

    proxy :: Proxy GsdWriteApi
    proxy = Proxy

    server :: EventStoreClientManager  -> Server GsdWriteApi
    server eventStoreClientManager = sendGsdCommand :<|> waitTillCommandResponseProduced
     where
      sendGsdCommand :: GsdCommand -> Handler PersistCommandResult
      sendGsdCommand gsdCommand = (liftIO $ Gsd.Write.persistCommand eventStoreClientManager gsdCommand )


      waitTillCommandResponseProduced :: AggregateId -> Offset -> CommandId -> Handler (SafeResponse (Persisted CommandResponse))
      waitTillCommandResponseProduced aggregateId offset commandId =
        liftIO $ Gsd.Write.waitTillCommandResponseProduced eventStoreClientManager aggregateId offset commandId

