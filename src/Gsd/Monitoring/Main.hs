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

module Gsd.Monitoring.Main (execute) where


import Servant
import Network.Wai.Handler.Warp

import Prelude hiding (foldr)
import Logger.Core

import qualified Database.EventStore as EventStore
import Control.Exception hiding (Handler)

import Gsd.Monitoring.WebStreamingApiDefinition

import PersistedStreamEngine.Instances.EventStore.EventStoreSettings

import DevOps.Core
import DevOps.MicroService.EventStore
import Control.Concurrent
import Gsd.Monitoring.Server
type ApiPort = Int


execute :: ApiPort -> EventStoreMicroService -> IO ()
execute apiPort eventStoreMicroService = do
  let logger = Logger { loggerId = "[gsd.monitoring.api]" , executableName = "monitoring.api" }
      eventStoreSettings = getEventStoreSettings eventStoreMicroService
      eventStoreConnectionType = getConnectionType eventStoreMicroService
      credentials = getCredentials eventStoreMicroService

  initLogger logger
  logInfo logger "Checking Service Health"
  waitTillMicroServiceHealthy logger eventStoreMicroService
  logInfo logger "Service is Up and Running "

--  catch
  (bracket
    (EventStore.connect eventStoreSettings eventStoreConnectionType)
    (\connection -> do EventStore.shutdown connection
                       EventStore.waitTillClosed connection)
    (\connection -> run apiPort $
                      serve
                      proxy $
                      server EventStoreSettings {logger, credentials, connection}))
--    (\e @SomeException {} -> do
--        logInfo logger $ "An Exception occured causing the reboot of the microservice : " ++ show e
--        execute apiPort eventStoreMicroService)


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

