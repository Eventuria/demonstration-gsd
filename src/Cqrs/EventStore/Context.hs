module Cqrs.EventStore.Context where

import Cqrs.Logger
import qualified Database.EventStore as EventStore

data EventStoreContext = Context { logger :: Logger,
                                  credentials :: EventStore.Credentials,
                                  connection :: EventStore.Connection}
