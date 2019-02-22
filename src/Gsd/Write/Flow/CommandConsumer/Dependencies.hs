{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Write.Flow.CommandConsumer.Dependencies  where

import Eventuria.Commons.Logger.Core
import qualified PersistedStreamEngine.Instances.EventStore.Client.Dependencies as EventStoreClient
import Gsd.Write.Flow.CommandConsumer.Settings
import Eventuria.Commons.Dependencies.Core
import Eventuria.Commons.Network.Core

data Dependencies = Dependencies {logger :: Logger ,
                                  port :: URLPort,
                                  eventStoreClientDependencies :: EventStoreClient.Dependencies}


retrieveDependencies :: RetrieveDependencies Settings Dependencies c
retrieveDependencies Settings {
                  serviceLoggerId,
                  port,
                  eventStoreClientSettings}
                executionUnderDependenciesAcquired
                executionIfDependenciesAcquisitionFailed  = do
  logger <- getLogger serviceLoggerId
  EventStoreClient.retrieveDependencies
    eventStoreClientSettings
    (\eventStoreClientDependencies -> executionUnderDependenciesAcquired Dependencies {..})
    (\unhealthyDependency -> executionIfDependenciesAcquisitionFailed unhealthyDependency)
