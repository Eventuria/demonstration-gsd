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

import           Prelude hiding (foldr)

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Concurrent
import           Control.Exception hiding (Handler)

import           Servant
import           Servant.Pipes ()
import           Network.Wai.Handler.Warp hiding (Settings)

import           Eventuria.Adapters.Servant.Wrapper

import           Eventuria.Commons.Logger.Core
import           Eventuria.Commons.DevOps.Core
import           Eventuria.Commons.Network.Core
import           Eventuria.Commons.System.Threading
import           Eventuria.Commons.Dependencies.HealthChecking

import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Offset

import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import           Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId
import           Eventuria.Libraries.CQRS.Write.Serialization.PersistenceResult ()
import           Eventuria.Libraries.CQRS.Write.PersistCommandResult
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId


import qualified Eventuria.GSD.Write.CommandSourcer.Service.OverEventStore as Service
import qualified Eventuria.GSD.Write.CommandSourcer.Server.Dependencies    as Server
import           Eventuria.GSD.Write.Model.Commands.Command
import           Eventuria.GSD.Write.CommandSourcer.Definition
import           Eventuria.GSD.Write.Model.Commands.Serialization ()
import           Eventuria.GSD.Write.CommandSourcer.Server.Settings


start :: Settings -> IO ()
start settings @ Settings {healthCheckLoggerId}  =
  waitTillHealthy
      healthCheckLoggerId
      settings
      Server.getDependencies
      Server.healthCheck >>
  catch
      (Server.getDependencies
         settings
         (runServerOnWarp))
      (\ServerDownException -> start settings)


  where
    runServerOnWarp :: Server.Dependencies -> IO()
    runServerOnWarp dependencies @ Server.Dependencies {logger,port} = do
           logInfo logger "Server Up and Running"
           serverThreadId <- myThreadId
           serverDownExceptionReceiver <- catchingServerDownExceptionOnceAndThenDiscard serverThreadId
           run port $ application
                        (proxy :: Proxy GsdWriteApi)
                        (writeServer serverDownExceptionReceiver)
                        dependencies

    {-- N.B : Servant does not support Streamly,
              so Streamly is converted to Pipe at the Servant Level (see toPipes )
    --}
    writeServer :: ServerThreadId -> ServantServer GsdWriteApi Server.Dependencies
    writeServer serverThreadId dependencies =
        healthCheck                            serverThreadId dependencies
          :<|> sendGsdCommand                  serverThreadId dependencies
          :<|> waitTillCommandResponseProduced serverThreadId dependencies
     where
      healthCheck :: ServerThreadId -> Server.Dependencies -> Handler Healthy
      healthCheck serverThreadId Server.Dependencies {logger} =
        liftIO $ logInfo logger "service health asked"  >>
                 Server.healthCheck dependencies >>=
                 either
                   (\error -> do
                       logInfo logger $ "service unhealthy : " ++ show error
                       return $ Left $ toException ServerDownException)
                   (\right -> do
                       logInfo logger "service healthy"
                       return $ Right ()) >>=
                 breakServerOnFailure logger serverThreadId

      sendGsdCommand :: ServerThreadId ->
                        Server.Dependencies ->
                        GsdCommand ->
                        Handler PersistCommandResult
      sendGsdCommand serverThreadId
                     Server.Dependencies {logger,eventStoreClientDependencies}
                     gsdCommand =
         liftIO $ (logInfo logger $ "command received : " ++ show gsdCommand)  >>
                  Service.persistCommand eventStoreClientDependencies gsdCommand >>=
                  breakServerOnFailure logger serverThreadId


      waitTillCommandResponseProduced :: ServerThreadId ->
                                         Server.Dependencies ->
                                         AggregateId ->
                                         Offset ->
                                         CommandId ->
                                         Handler (Persisted CommandResponse)
      waitTillCommandResponseProduced serverThreadId
                                      Server.Dependencies {logger,eventStoreClientDependencies}
                                      aggregateId
                                      offset
                                      commandId =
        liftIO $ (logInfo logger $ "waiting command response for command " ++ show commandId)  >>
                 Service.waitTillCommandResponseProduced
                              eventStoreClientDependencies
                              aggregateId
                              offset
                              commandId >>=
                 breakServerOnFailure logger serverThreadId

