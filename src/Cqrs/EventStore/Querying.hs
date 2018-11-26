{-# LANGUAGE TypeFamilies #-}
module Cqrs.EventStore.Querying where

import Data.Maybe
import Cqrs.EventStore.Stream
import qualified Database.EventStore as EventStore
import Control.Concurrent.Async (wait)
import Cqrs.EventStore.PersistedItem
import Cqrs.EventStore.Context
import Data.Aeson

retrieveLast :: FromJSON item => EventStoreStream item -> IO( Maybe (Persisted item))
retrieveLast EventStoreStream { context = Context { logger = logger,
                                                    credentials = credentials,
                                                    connection = connection },
                                streamName = streamName} =  do
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


recordedEventToPersistedItem :: FromJSON item => EventStore.RecordedEvent -> Persisted item
recordedEventToPersistedItem recordedEvent =
  PersistedItem { offset = toInteger $ EventStore.recordedEventNumber recordedEvent,
                  item = fromJust $ EventStore.recordedEventDataAsJson recordedEvent }

recordedEventToPersistedItems :: (EventStore.RecordedEvent -> persistedItem) -> EventStore.StreamSlice -> [persistedItem]
recordedEventToPersistedItems recordedEventToPersistedItem eventSlice = (\event -> recordedEventToPersistedItem $ EventStore.resolvedEventOriginal $ event)
                                                        <$> EventStore.sliceEvents eventSlice