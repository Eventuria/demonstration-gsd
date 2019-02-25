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


getDependencies :: RetrieveDependencies Settings Dependencies c
getDependencies Settings {
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


