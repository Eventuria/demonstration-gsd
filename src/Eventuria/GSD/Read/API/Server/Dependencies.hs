{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.GSD.Read.API.Server.Dependencies where

import Eventuria.Commons.Logger.Core
import Eventuria.Commons.Network.Core (URLPort)
import qualified Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Dependencies as EventStoreClient
import Eventuria.GSD.Read.API.Server.Settings
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

