module PersistedStreamEngine.Instances.EventStore.Read.CqrsInstance where

import PersistedStreamEngine.Interface.Read.Reading

import qualified PersistedStreamEngine.Instances.EventStore.Read.Streaming as EventStoreStreaming
import qualified PersistedStreamEngine.Instances.EventStore.Read.Querying as EventStoreQuerying
import qualified PersistedStreamEngine.Instances.EventStore.Read.Subscribing as EventStoreSubscribing

import PersistedStreamEngine.Instances.EventStore.EventStoreStream

getEventStoreStreaming :: Streaming EventStoreStream
getEventStoreStreaming = Streaming {
                  streamFromOffset = EventStoreStreaming.streamFromOffset,
                  streamAllInfinitely = EventStoreStreaming.streamAllInfinitely,
                  streamAll = EventStoreStreaming.streamAll,
                  streamAllSafe = EventStoreStreaming.streamAllSafe }

getEventStoreQuerying :: Querying EventStoreStream
getEventStoreQuerying = Querying  {
                  retrieveLast = EventStoreQuerying.retrieveLast,
                  isStreamNotFound = EventStoreQuerying.isStreamNotFound}

getEventStoreSubscribing :: Subscribing EventStoreStream
getEventStoreSubscribing = Subscribing {
                            subscribe = EventStoreSubscribing.subscribe,
                            subscribeOnOffset = EventStoreSubscribing.subscribeOnOffset}


getEventStoreReading :: Reading EventStoreStream
getEventStoreReading = Reading { streaming = getEventStoreStreaming, querying = getEventStoreQuerying, subscribing = getEventStoreSubscribing}
