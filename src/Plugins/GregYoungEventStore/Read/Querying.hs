{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Plugins.GregYoungEventStore.Read.Querying where

import Data.Maybe
import Plugins.GregYoungEventStore.EventStoreStream
import qualified Database.EventStore as EventStore
import Control.Concurrent.Async (wait)
import Cqrs.PersistedStream.PersistedItem
import Plugins.GregYoungEventStore.EventStoreSettings
import Data.Aeson
import Cqrs.PersistedStream.Offset

isStreamNotFound :: EventStoreStream item -> IO Bool
isStreamNotFound EventStoreStream { settings = EventStoreSettings { logger, credentials, connection },
                                           streamName = streamName} = do
   let resolveLinkTos = False

   asyncRead <- EventStore.readStreamEventsForward
                    connection
                    streamName
                    (fromInteger 0)
                    (fromInteger 1)
                    resolveLinkTos
                    (Just credentials)
   commandFetched <- wait asyncRead
   return $ case commandFetched of
        EventStore.ReadNoStream -> True
        _ -> False

retrieveLast :: FromJSON item => EventStoreStream item -> IO( Maybe (Persisted item))
retrieveLast EventStoreStream { settings = EventStoreSettings { logger, credentials, connection },
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

