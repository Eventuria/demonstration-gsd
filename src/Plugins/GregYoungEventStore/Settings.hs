module Plugins.GregYoungEventStore.Settings where

import Logger.Core
import qualified Database.EventStore as EventStore

data EventStoreSettings = EventStoreSettings {
                                  logger :: Logger,
                                  credentials :: EventStore.Credentials,
                                  connection :: EventStore.Connection}

