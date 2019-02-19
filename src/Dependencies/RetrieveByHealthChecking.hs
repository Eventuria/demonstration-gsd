{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Dependencies.RetrieveByHealthChecking where


import Logger.Core
import Control.Concurrent
import Time.Core
import Dependencies.Core
import Data.Function ((&))

checkHealthAndRetrieveDependencies ::
         LoggerId ->
         settings ->
         RetrieveDependencies settings dependencies c ->
         ExecutionUnderDependenciesAcquired dependencies c ->
         IO c
checkHealthAndRetrieveDependencies healthCheckLoggerId
                                   settings
                                   retrieveDependencies
                                   executionUnderDependenciesAcquired = do
  healthCheckLogger <- getLogger healthCheckLoggerId
  logInfo healthCheckLogger "------------------------------------------"
  logInfo healthCheckLogger "Service Health Checking"
  logInfo healthCheckLogger "------------------------------------------"
  checkDependenciesHealth
    healthCheckLogger
    settings
    retrieveDependencies
    executionUnderDependenciesAcquired
  where
    checkDependenciesHealth :: Logger ->
                               settings ->
                               RetrieveDependencies settings dependencies c ->
                               ExecutionUnderDependenciesAcquired dependencies c ->
                               IO c
    checkDependenciesHealth healthCheckLogger
                            settings
                            retrieveDependencies
                            executionUnderDependenciesAcquired =
        retrieveDependencies
          settings
          (\dependencies -> do
            logInfo healthCheckLogger "------------------------------------------"
            logInfo healthCheckLogger " [√] Service is up and running"
            logInfo healthCheckLogger "------------------------------------------"
            executionUnderDependenciesAcquired dependencies)
          (\unhealthyDependencies -> do
              logInfo healthCheckLogger "> The Service can't be up and running."
              logInfo healthCheckLogger "> Some Dependencies are unhealthy :"
              unhealthyDependencies & mapM_ (\UnhealthyDependency {name} ->
                logInfo healthCheckLogger $ "    [x] " ++ name ++ " Service")
              logInfo healthCheckLogger "------------------------------------------"
              logInfo healthCheckLogger "Retrying in 10s"
              logInfo healthCheckLogger "------------------------------------------"
              threadDelay $ getInMsFromSeconds 10
              checkDependenciesHealth
                healthCheckLogger
                settings
                retrieveDependencies
                executionUnderDependenciesAcquired)
