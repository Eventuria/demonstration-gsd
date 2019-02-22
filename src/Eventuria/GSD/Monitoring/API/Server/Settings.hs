{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.GSD.Monitoring.API.Server.Settings where

import Eventuria.Commons.Logger.Core
import Eventuria.Commons.Network.Core
import qualified Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Settings as EventStoreClient


data Settings = Settings { serviceLoggerId :: LoggerId,
                           healthCheckLoggerId :: LoggerId,
                           port :: URLPort,
                           eventStoreClientSettings :: EventStoreClient.Settings}



