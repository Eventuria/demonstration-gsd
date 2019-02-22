{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Read.API.Server.Dependencies where

import Eventuria.Commons.Logger.Core
import Eventuria.Commons.Network.Core (URLPort)
import qualified PersistedStreamEngine.Instances.EventStore.Client.Dependencies as EventStoreClient
import Gsd.Read.API.Server.Settings
import Eventuria.Commons.Dependencies.Core

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

