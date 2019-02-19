{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Write.Command.Consumer.Settings where

import Logger.Core
import qualified PersistedStreamEngine.Instances.EventStore.Client.Settings as EventStore

data Settings = Settings { serviceLoggerId :: LoggerId,
                           healthCheckLoggerId :: LoggerId,
                           eventStoreClientSettings :: EventStore.Settings}

