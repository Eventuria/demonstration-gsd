{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Write.Command.Consumer.CommandConsumer where

import Logger.Core
import qualified Gsd.Write.Service.OverEventStore as Gsd.Write
import qualified Gsd.Write.Command.Consumer.Settings as Consumer
import qualified Gsd.Write.Command.Consumer.Dependencies as Consumer
import Dependencies.RetrieveByHealthChecking

start :: Consumer.Settings -> IO ()
start settings @ Consumer.Settings {healthCheckLoggerId}  =
  checkHealthAndRetrieveDependencies
    healthCheckLoggerId
    settings
    Consumer.retrieveDependencies
    (\Consumer.Dependencies {logger, eventStoreClientDependencies } -> do
        logInfo logger "Starting Command Consumer"
        safeResponse <- Gsd.Write.startCommandConsumption logger eventStoreClientDependencies
        either
          (\error -> do
              logInfo logger $ "error : " ++ (show error)
              return ())
          (\right -> return ())
          safeResponse)

