{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Cqrs.EventStore.Stream where

import qualified Database.EventStore as EventStore
import Cqrs.EventStore.Context



data EventStoreStream persistedItem = EventStoreStream {
                                           context :: EventStoreContext,
                                           streamName :: EventStore.StreamName,
                                           recordedEventToPersistedItem :: (EventStore.RecordedEvent -> persistedItem)}


