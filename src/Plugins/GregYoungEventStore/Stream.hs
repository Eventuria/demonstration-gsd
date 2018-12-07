{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Plugins.GregYoungEventStore.Stream where

import qualified Database.EventStore as EventStore
import Plugins.GregYoungEventStore.Settings

data EventStoreStream item = EventStoreStream {
                                           settings :: EventStoreSettings,
                                           streamName :: EventStore.StreamName}


