module EventStore.Settings where

import Logger.Core
import qualified Database.EventStore as EventStore

data EventStoreContext = Context { logger :: Logger,
                                  credentials :: EventStore.Credentials,
                                  connection :: EventStore.Connection}

