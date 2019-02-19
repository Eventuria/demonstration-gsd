{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Monitoring.API.Server.Settings where

import Logger.Core
import Network.Core
import qualified PersistedStreamEngine.Instances.EventStore.Client.Settings as EventStoreClient


data Settings = Settings { serviceLoggerId :: LoggerId,
                           healthCheckLoggerId :: LoggerId,
                           port :: URLPort,
                           eventStoreClientSettings :: EventStoreClient.Settings}



