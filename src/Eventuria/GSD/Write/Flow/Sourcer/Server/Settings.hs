{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.GSD.Write.Flow.Sourcer.Server.Settings where

import Eventuria.Commons.Logger.Core
import Eventuria.Commons.Network.Core
import qualified Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Settings as EventStore


data Settings = Settings {serviceLoggerId :: LoggerId,
                          healthCheckLoggerId :: LoggerId,
                          port :: URLPort,
                          eventStoreClientSettings :: EventStore.Settings}