{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
module Cqrs.PersistedStream.Stream where

import Data.Aeson

import Streamly.Streamable

import Cqrs.Streams
import Cqrs.PersistedStream.PersistedItem

import Plugins.GregYoungEventStore.Stream


data Reading = Reading {
    streaming :: Streaming,
    querying :: Querying ,
    subscribing :: Subscribing }

data Streaming = Streaming { streamFromOffset    :: forall stream monad item . Streamable stream monad item => EventStoreStream item -> Offset -> stream monad (Persisted item),
                             streamAllInfinitely :: forall stream monad item . Streamable stream monad item => EventStoreStream item -> stream monad (Persisted item)}

data Querying = Querying { retrieveLast :: forall item . FromJSON item => EventStoreStream item -> IO( Maybe (Persisted item)) }

data Subscribing = Subscribing { subscribe :: forall stream monad item . Streamable stream monad item => EventStoreStream item -> stream monad (Persisted item) }
