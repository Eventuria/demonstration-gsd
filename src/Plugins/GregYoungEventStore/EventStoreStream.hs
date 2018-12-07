{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Plugins.GregYoungEventStore.EventStoreStream where

import qualified Database.EventStore as EventStore
import Plugins.GregYoungEventStore.EventStoreSettings

data EventStoreStream item = EventStoreStream {
                                           settings :: EventStoreSettings,
                                           streamName :: EventStore.StreamName}


