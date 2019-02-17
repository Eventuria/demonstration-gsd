{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module PersistedStreamEngine.Instances.EventStore.EventStoreStream where

import qualified Database.EventStore as EventStore
import PersistedStreamEngine.Instances.EventStore.EventStoreClientState

data EventStoreStream item = EventStoreStream {
                                           settings :: EventStoreClientState,
                                           streamName :: EventStore.StreamName}


