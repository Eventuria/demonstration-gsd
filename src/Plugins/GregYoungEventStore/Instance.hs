
module Plugins.GregYoungEventStore.Instance where

import Cqrs.PersistedStream.Stream

import qualified Plugins.GregYoungEventStore.Read.Streaming as EventStoreStreaming
import qualified Plugins.GregYoungEventStore.Read.Querying as EventStoreQuerying
import qualified Plugins.GregYoungEventStore.Read.Subscribing as EventStoreSubscribing



getStreaming :: Streaming
getStreaming = Streaming {
                  streamFromOffset = EventStoreStreaming.streamFromOffset,
                  streamAllInfinitely = EventStoreStreaming.streamAllInfinitely  }

getQuerying :: Querying
getQuerying = Querying  {retrieveLast = EventStoreQuerying.retrieveLast}

getSubscribing :: Subscribing
getSubscribing = Subscribing { subscribe = EventStoreSubscribing.subscribe}


getEventStoreReading :: Reading
getEventStoreReading = Reading { streaming = getStreaming, querying = getQuerying, subscribing = getSubscribing}