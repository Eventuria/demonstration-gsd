{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
module PersistedStreamEngine.Read.Interface where

import Data.Aeson

import Streamly.Streamable

import PersistedStreamEngine.Offset
import PersistedStreamEngine.PersistedItem

data Reading persistedStream = Reading {
    streaming :: Streaming persistedStream,
    querying :: Querying persistedStream,
    subscribing :: Subscribing persistedStream }

data Streaming persistedStream = Streaming {
                             streamFromOffset    :: forall stream monad item . Streamable stream monad item => persistedStream item -> Offset -> stream monad (Persisted item),
                             streamAllInfinitely :: forall stream monad item . Streamable stream monad item => persistedStream item -> stream monad (Persisted item) ,
                             streamAll ::           forall stream monad item . Streamable stream monad item => persistedStream item -> stream monad (Persisted item)}

data Querying persistedStream = Querying {
                             retrieveLast :: forall item . FromJSON item => persistedStream item -> IO( Maybe (Persisted item)),
                             isStreamNotFound :: forall item . persistedStream item -> IO Bool}

data Subscribing persistedStream = Subscribing { subscribe :: forall stream monad item . Streamable stream monad item => persistedStream item -> stream monad (Persisted item) }



