{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Write.Command.Consumer.Dependencies  where

import Logger.Core
import qualified PersistedStreamEngine.Instances.EventStore.Client.Dependencies as EventStoreClient
import Gsd.Write.Command.Consumer.Settings
import Dependencies.Core


data Dependencies = Dependencies {logger :: Logger ,
                                  eventStoreClientDependencies :: EventStoreClient.Dependencies}


retrieveDependencies :: RetrieveDependencies Settings Dependencies c
retrieveDependencies Settings {
                  serviceLoggerId,
                  eventStoreClientSettings}
                executionUnderDependenciesAcquired
                executionIfDependenciesAcquisitionFailed  = do
  logger <- getLogger serviceLoggerId
  EventStoreClient.retrieveDependencies
    eventStoreClientSettings
    (\eventStoreClientDependencies -> executionUnderDependenciesAcquired Dependencies {..})
    (\unhealthyDependency -> executionIfDependenciesAcquisitionFailed unhealthyDependency)
