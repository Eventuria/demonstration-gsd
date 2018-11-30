{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module EventStore.Stream where

import qualified Database.EventStore as EventStore
import EventStore.Settings

data EventStoreStream item = EventStoreStream {
                                           context :: EventStoreContext,
                                           streamName :: EventStore.StreamName}


