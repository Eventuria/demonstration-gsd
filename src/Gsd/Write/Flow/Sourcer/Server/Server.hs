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
module Gsd.Write.Flow.Sourcer.Server.Server (start) where

import Prelude hiding (foldr)
import Logger.Core
import Servant.Pipes ()

import Control.Monad.IO.Class (MonadIO(..))
import qualified Gsd.Write.Flow.Sourcer.Service.OverEventStore as Gsd.Write
import CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import CQRS.Write.Aggregate.Ids.AggregateId

import CQRS.Write.Serialization.PersistenceResult ()
import Gsd.Write.Flow.Sourcer.Definition
import Servant
import Servant.Wrapper
import Network.Wai.Handler.Warp hiding (Settings)
import Gsd.Write.Model.Commands.Command
import Gsd.Write.Model.Commands.Serialization ()
import CQRS.Write.PersistCommandResult
import PersistedStreamEngine.Interface.PersistedItem
import CQRS.Write.Aggregate.Commands.CommandId
import PersistedStreamEngine.Interface.Offset
import System.SafeResponse
import Gsd.Write.Flow.Sourcer.Server.Settings
import qualified Gsd.Write.Flow.Sourcer.Server.Dependencies as Server
import DevOps.Core
import Dependencies.RetrieveByHealthChecking

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
                        (proxy :: Proxy GsdWriteApi)
                        writeServer
                        dependencies

    {-- N.B : Servant does not support Streamly,
              so Streamly is converted to Pipe at the Servant Level (see toPipes )
    --}
    writeServer :: ServantServer GsdWriteApi Server.Dependencies
    writeServer dependencies = healthCheck
                          :<|> sendGsdCommand                  dependencies
                          :<|> waitTillCommandResponseProduced dependencies
     where
      healthCheck :: Handler HealthCheckResult
      healthCheck = return healthy

      sendGsdCommand :: Server.Dependencies -> GsdCommand -> Handler PersistCommandResult
      sendGsdCommand Server.Dependencies {eventStoreClientDependencies}
                     gsdCommand = (liftIO $ Gsd.Write.persistCommand eventStoreClientDependencies gsdCommand )


      waitTillCommandResponseProduced :: Server.Dependencies ->
                                         AggregateId ->
                                         Offset ->
                                         CommandId ->
                                         Handler (SafeResponse (Persisted CommandResponse))
      waitTillCommandResponseProduced Server.Dependencies {eventStoreClientDependencies}
                                      aggregateId
                                      offset
                                      commandId =
        liftIO $ Gsd.Write.waitTillCommandResponseProduced
                              eventStoreClientDependencies
                              aggregateId
                              offset
                              commandId

