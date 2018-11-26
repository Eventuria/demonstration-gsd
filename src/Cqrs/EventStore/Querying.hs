{-# LANGUAGE TypeFamilies #-}
module Cqrs.EventStore.Querying where

import Data.Maybe
import Cqrs.EventStore.Stream
import qualified Database.EventStore as EventStore
import Control.Concurrent.Async (wait)
import Cqrs.EventStore.PersistedItem
import Cqrs.EventStore.Context


retrieveLast :: EventStoreStream item -> IO( Maybe (Persisted item))
retrieveLast EventStoreStream { context = Context { logger = logger,
                                                    credentials = credentials,
                                                    connection = connection },
                                streamName = streamName,
                                recordedEventToPersistedItem = recordedEventToPersistedItem } =  do
        let resolveLinkTos = False
        readResult <- EventStore.readStreamEventsBackward
                    connection
                    streamName
                    (fromInteger 0)
                    (fromInteger 1)
                    resolveLinkTos
                    (Just credentials) >>= wait
        case readResult of
          EventStore.ReadSuccess responseContent -> do
              let snapshots = recordedEventToPersistedItems recordedEventToPersistedItem responseContent
              return $ listToMaybe snapshots
          EventStore.ReadNoStream -> return Nothing
          e -> error $ "Read failure: " <> show e

recordedEventToPersistedItems :: (EventStore.RecordedEvent -> persistedItem) -> EventStore.StreamSlice -> [persistedItem]
recordedEventToPersistedItems recordedEventToPersistedItem eventSlice = (\event -> recordedEventToPersistedItem $ EventStore.resolvedEventOriginal $ event)
                                                        <$> EventStore.sliceEvents eventSlice