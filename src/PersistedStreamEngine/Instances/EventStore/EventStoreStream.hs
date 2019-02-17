{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module PersistedStreamEngine.Instances.EventStore.EventStoreStream where

import qualified Database.EventStore as EventStore
import PersistedStreamEngine.Instances.EventStore.EventStoreClientManager

data EventStoreStream item = EventStoreStream {
                                           settings :: EventStoreClientManager,
                                           streamName :: EventStore.StreamName}


