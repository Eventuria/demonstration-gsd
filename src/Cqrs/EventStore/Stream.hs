{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Cqrs.EventStore.Stream where

import qualified Database.EventStore as EventStore
import Cqrs.EventStore.Context
import Cqrs.EventStore.PersistedItem


data EventStoreStream item = EventStoreStream {
                                           context :: EventStoreContext,
                                           streamName :: EventStore.StreamName,
                                           recordedEventToPersistedItem :: (EventStore.RecordedEvent -> Persisted item)}


