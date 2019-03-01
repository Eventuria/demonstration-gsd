{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.GSD.Write.CommandConsumer.Dependencies  where

import Eventuria.Commons.Logger.Core
import qualified Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Dependencies as EventStoreClient
import Eventuria.GSD.Write.CommandConsumer.Settings
import Eventuria.Commons.Dependencies.Core
import Eventuria.Commons.Network.Core

data Dependencies = Dependencies {logger :: Logger ,
                                  port :: URLPort,
                                  eventStoreClientDependencies :: EventStoreClient.Dependencies}


getDependencies :: GetDependencies Settings Dependencies c
getDependencies Settings {
                  serviceLoggerId,
                  port,
                  eventStoreClientSettings}
                executionUnderDependenciesAcquired = do
  logger <- getLogger serviceLoggerId
  EventStoreClient.getDependencies
    eventStoreClientSettings
    (\eventStoreClientDependencies -> executionUnderDependenciesAcquired Dependencies {..})


healthCheck :: HealthCheck Dependencies
healthCheck dependencies @ Dependencies {eventStoreClientDependencies}  = do
  result <- EventStoreClient.healthCheck eventStoreClientDependencies
  return $ fmap (\_ -> ()) result
