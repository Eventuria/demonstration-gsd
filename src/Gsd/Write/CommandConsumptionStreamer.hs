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

execute :: EventStoreMicroService -> IO (SafeResponse ())
execute eventStoreMicroService = do
  let logger = Logger { loggerId = "[gsd.command.processing.manager]" , executableName = "command.processing.manager" }
      eventStoreSettings = getEventStoreSettings eventStoreMicroService
      eventStoreConnectionType = getConnectionType eventStoreMicroService
      credentials = getCredentials eventStoreMicroService

  initLogger logger

  logInfo logger "Checking Service Health"
  waitTillMicroServiceHealthy logger eventStoreMicroService
  logInfo logger "Service is Up and Running "

  catch
    (bracket (EventStore.connect eventStoreSettings eventStoreConnectionType)
           (\connection -> do EventStore.shutdown connection
                              EventStore.waitTillClosed connection)
           (\connection ->do
              let eventStoreSettings = EventStoreSettings {logger = logger, credentials = credentials, connection = connection}
              Gsd.Write.startCommandConsumption eventStoreSettings logger ))
    (\e @SomeException {} -> do
        logInfo logger $ "An Exception occured causing the reboot of the microservice : " ++ show e
        execute eventStoreMicroService)


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
