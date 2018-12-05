{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Plugins.GregYoungEventStore.Read.Querying where

import Data.Maybe
import Plugins.GregYoungEventStore.Stream
import qualified Database.EventStore as EventStore
import Control.Concurrent.Async (wait)
import Cqrs.PersistedStream.PersistedItem
import Plugins.GregYoungEventStore.Settings
import Data.Aeson
import Cqrs.Streams

retrieveLast :: FromJSON item => EventStoreStream item -> IO( Maybe (Persisted item))
retrieveLast EventStoreStream { context = Context { logger = logger,
                                                    credentials = credentials,
                                                    connection = connection },
                                streamName = streamName} =  do
        let resolveLinkTos = False
        readResult <- EventStore.readEvent
                    connection
                    streamName
                    (fromInteger (-1)) -- constant for StreamPositionEnd
                    resolveLinkTos
                    (Just credentials) >>= wait
        return $ case readResult of
          EventStore.ReadSuccess EventStore.ReadEvent {readEventResolved = readEventResolved , readEventNumber = readEventNumber} -> do
             Just $ recordedEventToPersistedItem (toInteger $ readEventNumber) readEventResolved
          EventStore.ReadNoStream ->
             Nothing
          e -> error $ "retrieveLast failure: " <> show e


recordedEventToPersistedItem :: FromJSON item => Offset -> EventStore.ResolvedEvent -> Persisted item
recordedEventToPersistedItem offset readEventResolved =
  PersistedItem { offset = offset,
                  item = fromJust $ EventStore.resolvedEventDataAsJson readEventResolved }

