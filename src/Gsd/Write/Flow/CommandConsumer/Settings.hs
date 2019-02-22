{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Write.Flow.CommandConsumer.Settings where

import Eventuria.Commons.Logger.Core
import Eventuria.Commons.Network.Core
import qualified PersistedStreamEngine.Instances.EventStore.Client.Settings as EventStore

data Settings = Settings { serviceLoggerId :: LoggerId,
                           healthCheckLoggerId :: LoggerId,
                           port :: URLPort,
                           eventStoreClientSettings :: EventStore.Settings}

