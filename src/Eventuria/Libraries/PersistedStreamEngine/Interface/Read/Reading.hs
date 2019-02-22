{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
module Eventuria.Libraries.PersistedStreamEngine.Interface.Read.Reading where

import Data.Aeson

import Eventuria.Libraries.PersistedStreamEngine.Interface.Streamable

import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import Eventuria.Commons.System.SafeResponse

data Reading persistedStream = Reading {
    streaming :: Streaming persistedStream,
    querying :: Querying persistedStream,
    subscribing :: Subscribing persistedStream }

data Streaming persistedStream = Streaming {
           streamFromOffset    :: forall stream monad item . Streamable stream monad item =>
                                                              persistedStream item ->
                                                              Offset ->
                                                              stream monad (SafeResponse (Persisted item)),
           streamAllInfinitely :: forall stream monad item . Streamable stream monad item =>
                                                              persistedStream item ->
                                                              stream monad (SafeResponse (Persisted item)) ,
           streamAll ::           forall stream monad item . Streamable stream monad item =>
                                                              persistedStream item ->
                                                              stream monad (SafeResponse (Persisted item))}

data Querying persistedStream = Querying {
                             retrieveLast :: forall item . FromJSON item =>
                                                              persistedStream item ->
                                                              IO( SafeResponse (Maybe (Persisted item))),
                             isStreamNotFound :: forall item . persistedStream item -> IO (SafeResponse Bool)}

data Subscribing persistedStream = Subscribing {
                              subscribe :: forall stream monad item . Streamable stream monad item =>
                                                                          persistedStream item ->
                                                                          stream monad (SafeResponse (Persisted item)),
                              subscribeOnOffset :: forall  item. FromJSON item =>
                                                                  persistedStream item ->
                                                                  Offset ->
                                                                  IO (SafeResponse (Persisted item)) }



