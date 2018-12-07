
module Plugins.GregYoungEventStore.Instance where

import Cqrs.PersistedStream.Read.Interface
import Cqrs.PersistedStream.Write.Interface

import qualified Plugins.GregYoungEventStore.Read.Streaming as EventStoreStreaming
import qualified Plugins.GregYoungEventStore.Read.Querying as EventStoreQuerying
import qualified Plugins.GregYoungEventStore.Read.Subscribing as EventStoreSubscribing
import qualified Plugins.GregYoungEventStore.Write.Persisting as EventStoreWriting

import Plugins.GregYoungEventStore.Stream

getEventStoreStreaming :: Streaming EventStoreStream
getEventStoreStreaming = Streaming {
                  streamFromOffset = EventStoreStreaming.streamFromOffset,
                  streamAllInfinitely = EventStoreStreaming.streamAllInfinitely  }

getEventStoreQuerying :: Querying EventStoreStream
getEventStoreQuerying = Querying  {
                  retrieveLast = EventStoreQuerying.retrieveLast,
                  isStreamNotFound = EventStoreQuerying.isStreamNotFound}

getEventStoreSubscribing :: Subscribing EventStoreStream
getEventStoreSubscribing = Subscribing { subscribe = EventStoreSubscribing.subscribe}


getEventStoreReading :: Reading EventStoreStream
getEventStoreReading = Reading { streaming = getEventStoreStreaming, querying = getEventStoreQuerying, subscribing = getEventStoreSubscribing}

getEventStoreWriting :: Writing EventStoreStream
getEventStoreWriting = Writing {persist = EventStoreWriting.persist}