{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
module PersistedStreamEngine.Interface.Read.Reading where

import Data.Aeson

import PersistedStreamEngine.Interface.Streamable

import PersistedStreamEngine.Interface.Offset
import PersistedStreamEngine.Interface.PersistedItem

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

data Subscribing persistedStream = Subscribing {
                              subscribe :: forall stream monad item . Streamable stream monad item => persistedStream item -> stream monad (Persisted item),
                              subscribeOnOffset :: forall  item. FromJSON item => persistedStream item -> Offset -> IO (Persisted item) }


