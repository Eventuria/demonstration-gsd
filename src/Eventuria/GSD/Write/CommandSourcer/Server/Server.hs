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
module Eventuria.GSD.Write.CommandSourcer.Server.Server (start) where

import Prelude hiding (foldr)
import Eventuria.Commons.Logger.Core
import Servant.Pipes ()

import Control.Monad.IO.Class (MonadIO(..))
import qualified Eventuria.GSD.Write.CommandSourcer.Service.OverEventStore as Eventuria.GSD.Write
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId

import Eventuria.Libraries.CQRS.Write.Serialization.PersistenceResult ()
import Eventuria.GSD.Write.CommandSourcer.Definition
import Servant
import Eventuria.Adapters.Servant.Wrapper
import Network.Wai.Handler.Warp hiding (Settings)
import Eventuria.GSD.Write.Model.Commands.Command
import Eventuria.GSD.Write.Model.Commands.Serialization ()
import Eventuria.Libraries.CQRS.Write.PersistCommandResult
import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import Eventuria.Commons.System.SafeResponse
import Eventuria.GSD.Write.CommandSourcer.Server.Settings
import qualified Eventuria.GSD.Write.CommandSourcer.Server.Dependencies as Server
import Eventuria.Commons.DevOps.Core
import Eventuria.Commons.Dependencies.RetrieveByHealthChecking

start :: Settings -> IO ()
start settings @ Settings {healthCheckLoggerId}  =
  waitTillHealthy
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
      healthCheck = liftIO $ Server.retrieveDependencies
                                      settings
                                      (\dependencies -> return healthy)
                                      (\unhealthyDependencies -> return $ unhealthy "Service unavailable")

      sendGsdCommand :: Server.Dependencies -> GsdCommand -> Handler PersistCommandResult
      sendGsdCommand Server.Dependencies {eventStoreClientDependencies}
                     gsdCommand = (liftIO $ Eventuria.GSD.Write.persistCommand eventStoreClientDependencies gsdCommand )


      waitTillCommandResponseProduced :: Server.Dependencies ->
                                         AggregateId ->
                                         Offset ->
                                         CommandId ->
                                         Handler (SafeResponse (Persisted CommandResponse))
      waitTillCommandResponseProduced Server.Dependencies {eventStoreClientDependencies}
                                      aggregateId
                                      offset
                                      commandId =
        liftIO $ Eventuria.GSD.Write.waitTillCommandResponseProduced
                              eventStoreClientDependencies
                              aggregateId
                              offset
                              commandId

