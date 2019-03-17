module Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Read.CqrsInstance where

import Eventuria.Libraries.PersistedStreamEngine.Interface.Read.Reading

import qualified Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Read.Streaming as EventStoreStreaming
import qualified Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Read.Querying as EventStoreQuerying
import qualified Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Read.Subscribing as EventStoreSubscribing

import Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.EventStoreStream

getEventStoreStreaming :: Streaming EventStoreStream
getEventStoreStreaming = Streaming {
                  streamFromOffsetInclusive = EventStoreStreaming.streamFromOffsetInclusive,
                  streamFromRangeInclusive = EventStoreStreaming.streamFromRangeInclusive,
                  streamAllInfinitely = EventStoreStreaming.streamAllInfinitely,
                  streamAll = EventStoreStreaming.streamAll }

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
