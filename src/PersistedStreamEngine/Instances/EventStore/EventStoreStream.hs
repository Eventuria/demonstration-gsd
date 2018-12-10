{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module PersistedStreamEngine.Instances.EventStore.EventStoreStream where

import qualified Database.EventStore as EventStore
import PersistedStreamEngine.Instances.EventStore.EventStoreSettings

data EventStoreStream item = EventStoreStream {
                                           settings :: EventStoreSettings,
                                           streamName :: EventStore.StreamName}


