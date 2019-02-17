{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Monitoring.API.Server.Settings where

import Logger.Core
import Network.Core
import PersistedStreamEngine.Instances.EventStore.EventStoreClientSettings


data Settings = Settings {loggerId :: LoggerId,
                          port :: URLPort,
                          eventStoreClientSettings :: EventStoreClientSettings}

