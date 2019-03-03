{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.EventStoreStream where

import qualified Database.EventStore as EventStore
import Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Dependencies

data EventStoreStream item = EventStoreStream {
                                           clientDependencies :: Dependencies,
                                           streamName :: EventStore.StreamName}


