{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Monitoring.API.Server.Dependencies where

import Logger.Core
import Network.Core
import qualified PersistedStreamEngine.Instances.EventStore.Client.Dependencies as EventStoreClient
import Gsd.Monitoring.API.Server.Settings
import Dependencies.Core

data Dependencies = Dependencies { logger :: Logger ,
                                   port :: URLPort,
                                   eventStoreClientDependencies :: EventStoreClient.Dependencies}


retrieveDependencies :: RetrieveDependencies Settings Dependencies c
retrieveDependencies Settings {serviceLoggerId, eventStoreClientSettings,port}
                executionUnderDependenciesAcquired
                executionIfDependenciesAcquisitionFailed  = do
  logger <- getLogger serviceLoggerId
  EventStoreClient.retrieveDependencies
      eventStoreClientSettings
      (\eventStoreClientDependencies -> executionUnderDependenciesAcquired Dependencies {..})
      (\unhealthyDependency -> executionIfDependenciesAcquisitionFailed unhealthyDependency)