{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Write.CommandConsumptionStreamer where

import Logger.Core
import qualified Database.EventStore as EventStore
import Control.Exception
import DevOps.MicroService.EventStore
import DevOps.Core
import Control.Concurrent
import PersistedStreamEngine.Instances.EventStore.EventStoreSettings
import qualified Gsd.Write.GsdOverEventStore as Gsd.Write
import System.SafeResponse

execute :: EventStoreMicroService -> IO ()
execute eventStoreMicroService = do
  let logger = Logger { loggerId = "[gsd.command.processing.manager]" , executableName = "command.processing.manager" }
  initLogger logger

  logInfo logger "Checking Service Health"
  waitTillMicroServiceHealthy logger eventStoreMicroService
  logInfo logger "Service is Up and Running "

  safeResponse <- executeSafely logger eventStoreMicroService
  case safeResponse of
    Right () -> do
      logInfo logger $ "The microservice terminated without issues raised (it should not happened...)"
      return ()
    Left error -> do
      logInfo logger $ "An Exception occured causing the reboot of the microservice : " ++ show error
      execute eventStoreMicroService
  where
     executeSafely :: Logger -> EventStoreMicroService -> IO(SafeResponse())
     executeSafely logger eventStoreMicroService  = do
       catch
          (bracket ((EventStore.connect <$> getEventStoreSettings <*> getConnectionType) eventStoreMicroService )
              (\connection -> do EventStore.shutdown connection
                                 EventStore.waitTillClosed connection)
              (\connection -> Gsd.Write.startCommandConsumption
                                  EventStoreSettings {credentials = getCredentials eventStoreMicroService ,..}
                                  logger ))
          (\e @SomeException {} -> return $ Left e)


waitTillMicroServiceHealthy ::  Logger -> EventStoreMicroService -> IO ()
waitTillMicroServiceHealthy logger eventStoreMicroService = do

      healthCheckResult <- healthCheck eventStoreMicroService
      case (healthCheckResult) of
        Healthy -> do
          logInfo logger "EventStore Module : Healthy"
        Unhealthy reason -> do
          logInfo logger $ "EventStore Module : Unhealthy (" ++ reason ++ ")"
          threadDelay (5 * 1000000) -- 5 seconds
          logInfo logger "Retrying Health Check"
          waitTillMicroServiceHealthy logger eventStoreMicroService


