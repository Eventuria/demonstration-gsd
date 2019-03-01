{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.GSD.Monitoring.API.Server.Dependencies where

import Eventuria.Commons.Logger.Core
import Eventuria.Commons.Network.Core
import qualified Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Dependencies as EventStoreClient
import Eventuria.GSD.Monitoring.API.Server.Settings
import Eventuria.Commons.Dependencies.Core

data Dependencies = Dependencies { logger :: Logger ,
                                   port :: URLPort,
                                   eventStoreClientDependencies :: EventStoreClient.Dependencies}


getDependencies :: GetDependencies Settings Dependencies c
getDependencies Settings {serviceLoggerId, eventStoreClientSettings,port}
                executionUnderDependenciesAcquired = do
  logger <- getLogger serviceLoggerId
  EventStoreClient.getDependencies
      eventStoreClientSettings
      (\eventStoreClientDependencies -> executionUnderDependenciesAcquired Dependencies {..})


healthCheck :: HealthCheck Dependencies
healthCheck dependencies @ Dependencies {eventStoreClientDependencies}  = do
  result <- EventStoreClient.healthCheck eventStoreClientDependencies
  return $ fmap (\_ -> ()) result