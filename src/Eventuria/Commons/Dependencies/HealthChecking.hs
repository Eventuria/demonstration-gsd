{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.Commons.Dependencies.HealthChecking where

import Data.Function ((&))
import Control.Concurrent

import Eventuria.Adapters.Time.Core

import Eventuria.Commons.Dependencies.Core
import Eventuria.Commons.Logger.Core
import Data.List.NonEmpty

waitTillHealthy :: LoggerId ->
               settings ->
               GetDependencies settings dependencies (Either (NonEmpty UnhealthyDependency) Healthy) ->
               HealthCheck dependencies  ->
               IO ()
waitTillHealthy healthCheckLoggerId
            settings
            getDependencies
            healthCheck = do
  healthCheckLogger <- getLogger healthCheckLoggerId
  logInfo healthCheckLogger "------------------------------------------"
  logInfo healthCheckLogger "Service Health Checking"
  logInfo healthCheckLogger "------------------------------------------"
  checkDependenciesHealth
    healthCheckLogger
    settings
    getDependencies
    healthCheck
  where
    checkDependenciesHealth :: Logger ->
                               settings ->
                               GetDependencies settings dependencies (Either (NonEmpty UnhealthyDependency) Healthy) ->
                               HealthCheck dependencies  ->
                               IO ()
    checkDependenciesHealth healthCheckLogger
                            settings
                            getDependencies
                            healthCheck = do
        result <- getDependencies
          settings
          (\dependencies -> do
            result <- healthCheck dependencies
            either
              (\unhealthyDependencies -> do
                  logInfo healthCheckLogger "> The Service can't be up and running."
                  logInfo healthCheckLogger "> Some Dependencies are unhealthy :"
                  unhealthyDependencies & mapM_ (\UnhealthyDependency {name} ->
                    logInfo healthCheckLogger $ "    [x] " ++ name ++ " Service")
                  return $ Left unhealthyDependencies)
              (\dependencies -> do
                logInfo healthCheckLogger "------------------------------------------"
                logInfo healthCheckLogger " [√] Service is up and running"
                logInfo healthCheckLogger "------------------------------------------"
                return  $ Right ())
              result)
        either
         (\checkFailed -> do
              logInfo healthCheckLogger "------------------------------------------"
              logInfo healthCheckLogger "Retrying in 5s"
              logInfo healthCheckLogger "------------------------------------------"
              threadDelay $ getInMsFromSeconds 5
              checkDependenciesHealth
                healthCheckLogger
                settings
                getDependencies
                healthCheck)
         (\serviceUpAndRunning -> return ())
         result


