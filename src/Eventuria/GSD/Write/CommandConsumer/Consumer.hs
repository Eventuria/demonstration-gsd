{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.GSD.Write.CommandConsumer.Consumer where

import           Control.Concurrent
                 
import           Eventuria.Commons.Logger.Core
import           Eventuria.Commons.Dependencies.RetrieveByHealthChecking


import qualified Eventuria.GSD.Write.CommandConsumer.Service.OverEventStore as Consumer.Service
import qualified Eventuria.GSD.Write.CommandConsumer.Settings               as Consumer
import qualified Eventuria.GSD.Write.CommandConsumer.Dependencies           as Consumer
import qualified Eventuria.GSD.Write.CommandConsumer.API.HealthCheck.Server as Consumer.HealthCheck

start :: Consumer.Settings -> IO ()
start settings @ Consumer.Settings {healthCheckLoggerId}  =
  waitTillHealthy
    healthCheckLoggerId
    settings
    Consumer.getDependencies
    (\consumerDependencies-> do
      forkIO (Consumer.HealthCheck.runServerOnWarp settings consumerDependencies)
      startConsumer consumerDependencies)

 where
  startConsumer :: Consumer.Dependencies -> IO ()
  startConsumer Consumer.Dependencies {logger, eventStoreClientDependencies } = do
    logInfo logger "Starting Command Consumer"
    safeResponse <- Consumer.Service.consumeCommands logger eventStoreClientDependencies
    either
     (\error -> do
         logInfo logger $ "error : " ++ (show error)
         return ())
     (\right -> return ())
     safeResponse