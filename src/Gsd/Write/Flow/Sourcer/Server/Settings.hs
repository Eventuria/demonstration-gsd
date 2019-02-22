{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Write.Flow.Sourcer.Server.Settings where

import Logger.Core
import Network.Core
import qualified PersistedStreamEngine.Instances.EventStore.Client.Settings as EventStore


data Settings = Settings {serviceLoggerId :: LoggerId,
                          healthCheckLoggerId :: LoggerId,
                          port :: URLPort,
                          eventStoreClientSettings :: EventStore.Settings}