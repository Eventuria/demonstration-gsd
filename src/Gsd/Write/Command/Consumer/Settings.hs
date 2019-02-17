{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Write.Command.Consumer.Settings where

import Logger.Core
import PersistedStreamEngine.Instances.EventStore.EventStoreClientSettings

data Settings = Settings { loggerId :: LoggerId,
                           eventStoreClientSettings :: EventStoreClientSettings}

