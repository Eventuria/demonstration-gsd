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
import PersistedStreamEngine.Instances.EventStore.EventStoreClientState
import qualified Gsd.Write.Service.OverEventStore as Gsd.Write
import Cqrs.Write.Aggregate.Commands.Responses.CommandResponse
import Cqrs.Write.Aggregate.Ids.AggregateId

import Cqrs.Write.Serialization.PersistenceResult ()
import Gsd.Write.API.Definition
import Servant
import Network.Wai.Handler.Warp hiding (Settings)
import Gsd.Write.Model.Commands.Command
import Gsd.Write.Model.Commands.Serialization ()
import Cqrs.Write.PersistCommandResult
import PersistedStreamEngine.Interface.PersistedItem
import Cqrs.Write.Aggregate.Commands.CommandId
import PersistedStreamEngine.Interface.Offset
import System.SafeResponse
import Gsd.Write.API.Server.Settings
import qualified Gsd.Write.API.Server.State as Server.State
import Gsd.Write.API.Server.State


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

    proxy :: Proxy GsdWriteApi
    proxy = Proxy

    server :: EventStoreClientState  -> Server GsdWriteApi
    server eventStoreClientState = sendGsdCommand :<|> waitTillCommandResponseProduced
     where
      sendGsdCommand :: GsdCommand -> Handler PersistCommandResult
      sendGsdCommand gsdCommand = (liftIO $ Gsd.Write.persistCommand eventStoreClientState gsdCommand )


      waitTillCommandResponseProduced :: AggregateId ->
                                         Offset ->
                                         CommandId ->
                                         Handler (SafeResponse (Persisted CommandResponse))
      waitTillCommandResponseProduced aggregateId offset commandId =
        liftIO $ Gsd.Write.waitTillCommandResponseProduced
                              eventStoreClientState
                              aggregateId
                              offset
                              commandId

