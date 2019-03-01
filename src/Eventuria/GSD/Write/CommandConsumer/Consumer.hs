{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.GSD.Write.CommandConsumer.Consumer where

import           System.Exit (exitSuccess)
import           GHC.IO.Exception
import           Control.Concurrent
import           Control.Exception

import           Eventuria.Commons.Logger.Core
import           Eventuria.Commons.Dependencies.HealthChecking


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
        Consumer.healthCheck >>
    Consumer.getDependencies
         settings
         (\dependencies -> forkIO (Consumer.HealthCheck.runServerOnWarp dependencies) >> startConsumer dependencies)


 where
  startConsumer :: Consumer.Dependencies -> IO ()
  startConsumer Consumer.Dependencies {logger, eventStoreClientDependencies } =
    logInfo logger "Starting Command Consumer" >>
    Consumer.Service.consumeCommands logger eventStoreClientDependencies >>=
    either
     (\someException@ SomeException {} ->
         case (asyncExceptionFromException someException) of
           Just (UserInterrupt) -> do
              logInfo logger $ "receiving request to end the service."
              logInfo logger $ "shutting down the service"
              exitSuccess
           otherwise -> do
              logInfo logger $ "issue propagated : " ++ (show someException)
              start settings)
     (\end -> return ())
