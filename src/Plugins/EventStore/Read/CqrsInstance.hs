module Plugins.EventStore.Read.CqrsInstance where

import PersistedStreamEngine.Read.Interface

import qualified Plugins.EventStore.Read.Streaming as EventStoreStreaming
import qualified Plugins.EventStore.Read.Querying as EventStoreQuerying
import qualified Plugins.EventStore.Read.Subscribing as EventStoreSubscribing

import Plugins.EventStore.EventStoreStream

getEventStoreStreaming :: Streaming EventStoreStream
getEventStoreStreaming = Streaming {
                  streamFromOffset = EventStoreStreaming.streamFromOffset,
                  streamAllInfinitely = EventStoreStreaming.streamAllInfinitely,
                  streamAll = EventStoreStreaming.streamAll }

getEventStoreQuerying :: Querying EventStoreStream
getEventStoreQuerying = Querying  {
                  retrieveLast = EventStoreQuerying.retrieveLast,
                  isStreamNotFound = EventStoreQuerying.isStreamNotFound}

getEventStoreSubscribing :: Subscribing EventStoreStream
getEventStoreSubscribing = Subscribing { subscribe = EventStoreSubscribing.subscribe}


getEventStoreReading :: Reading EventStoreStream
getEventStoreReading = Reading { streaming = getEventStoreStreaming, querying = getEventStoreQuerying, subscribing = getEventStoreSubscribing}
