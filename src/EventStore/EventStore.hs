{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
module EventStore.EventStore where

import EventStore.Streamable
import EventStore.Stream
import EventStore.Read.PersistedItem
import qualified EventStore.Read.Streaming as Streaming
import qualified EventStore.Read.Querying as Querying
import qualified EventStore.Read.Subscribing as Subscribing
import Cqrs.Streams
import Data.Aeson

data EventStoreStreaming = Streaming { streamFromOffset ::    forall monad stream item . Streamable monad stream item => EventStoreStream item -> Offset -> stream monad (Persisted item),
                                     streamAllInfinitely :: forall monad stream item . Streamable monad stream item => EventStoreStream item -> stream monad (Persisted item)}

data EventStoreQuerying = Querying { retrieveLast :: forall item . FromJSON item => EventStoreStream item -> IO( Maybe (Persisted item)) }

data EventStoreSubscribing = Subscribing { subscribe :: forall monad stream item . Streamable monad stream item => EventStoreStream item -> stream monad (Persisted item) }

data EventStoreReading = Reading { streaming :: EventStoreStreaming, querying :: EventStoreQuerying , subscribing :: EventStoreSubscribing }

getStreaming :: EventStoreStreaming
getStreaming = Streaming {
                  streamFromOffset = Streaming.streamFromOffset,
                  streamAllInfinitely = Streaming.streamAllInfinitely  }

getQuerying :: EventStoreQuerying
getQuerying = Querying  {retrieveLast = Querying.retrieveLast}

getSubscribing :: EventStoreSubscribing
getSubscribing = Subscribing { subscribe = Subscribing.subscribe}


getEventStoreReading :: EventStoreReading
getEventStoreReading = Reading { streaming = getStreaming, querying = getQuerying, subscribing = getSubscribing}