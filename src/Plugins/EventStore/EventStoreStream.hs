{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Plugins.EventStore.EventStoreStream where

import qualified Database.EventStore as EventStore
import Plugins.EventStore.EventStoreSettings

data EventStoreStream item = EventStoreStream {
                                           settings :: EventStoreSettings,
                                           streamName :: EventStore.StreamName}


