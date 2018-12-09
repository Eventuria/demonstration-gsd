module Plugins.GregYoungEventStore.Read.CqrsInstance where

import Cqrs.PersistedStreamEngine.Read.Interface

import qualified Plugins.GregYoungEventStore.Read.Streaming as EventStoreStreaming
import qualified Plugins.GregYoungEventStore.Read.Querying as EventStoreQuerying
import qualified Plugins.GregYoungEventStore.Read.Subscribing as EventStoreSubscribing

import Plugins.GregYoungEventStore.EventStoreStream

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
