{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module PersistedStreamEngine.Instances.EventStore.Read.Querying where

import Data.Maybe
import PersistedStreamEngine.Instances.EventStore.EventStoreStream
import qualified Database.EventStore as EventStore
import Control.Concurrent.Async (wait)
import PersistedStreamEngine.Interface.PersistedItem
import PersistedStreamEngine.Instances.EventStore.EventStoreSettings
import Data.Aeson
import PersistedStreamEngine.Interface.Offset

isStreamNotFound :: EventStoreStream item -> IO Bool
isStreamNotFound EventStoreStream { settings = EventStoreSettings { logger, credentials, connection },
                                           streamName = streamName} = do


   asyncRead <- EventStore.readEventsForward
                    connection
                    streamName
                    EventStore.streamStart
                    1
                    EventStore.NoResolveLink
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
                    EventStore.streamEnd
                    EventStore.NoResolveLink
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

