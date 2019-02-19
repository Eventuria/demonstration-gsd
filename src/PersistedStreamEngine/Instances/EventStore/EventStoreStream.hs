{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module PersistedStreamEngine.Instances.EventStore.EventStoreStream where

import qualified Database.EventStore as EventStore
import PersistedStreamEngine.Instances.EventStore.Client.Dependencies

data EventStoreStream item = EventStoreStream {
                                           dependencies :: Dependencies,
                                           streamName :: EventStore.StreamName}


