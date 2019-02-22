{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.GSD.Write.Flow.CommandConsumer.Consumer where

import Eventuria.Commons.Logger.Core
import qualified Eventuria.GSD.Write.Flow.CommandConsumer.Service.OverEventStore as Eventuria.GSD.Write.Flow.CommandConsumer.Service
import qualified Eventuria.GSD.Write.Flow.CommandConsumer.Settings as Consumer
import qualified Eventuria.GSD.Write.Flow.CommandConsumer.Dependencies as Consumer
import Eventuria.Commons.Dependencies.RetrieveByHealthChecking
import Control.Concurrent
import qualified Eventuria.GSD.Write.Flow.CommandConsumer.API.HealthCheck.Server as Consumer.HealthCheck

start :: Consumer.Settings -> IO ()
start settings @ Consumer.Settings {healthCheckLoggerId}  =
  checkHealthAndRetrieveDependencies
    healthCheckLoggerId
    settings
    Consumer.retrieveDependencies
    (\consumerDependencies-> do
      forkIO (Consumer.HealthCheck.runServerOnWarp consumerDependencies)
      startConsumer consumerDependencies)

 where
  startConsumer :: Consumer.Dependencies -> IO ()
  startConsumer Consumer.Dependencies {logger, eventStoreClientDependencies } = do
    logInfo logger "Starting Command Consumer"
    safeResponse <- Eventuria.GSD.Write.Flow.CommandConsumer.Service.consumeCommands logger eventStoreClientDependencies
    either
     (\error -> do
         logInfo logger $ "error : " ++ (show error)
         return ())
     (\right -> return ())
     safeResponse