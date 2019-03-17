{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
module Eventuria.Libraries.PersistedStreamEngine.Interface.Read.Reading where

import Control.Exception

import Data.Aeson

import Eventuria.Libraries.PersistedStreamEngine.Interface.Streamable
import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem

data Reading persistedStream = Reading {
    streaming :: Streaming persistedStream,
    querying :: Querying persistedStream,
    subscribing :: Subscribing persistedStream }

data Streaming persistedStream = Streaming {
           streamFromOffsetInclusive    :: forall stream monad item . Streamable stream monad item =>
                                                              persistedStream item ->
                                                              Offset ->
                                                              stream monad (Either SomeException (Persisted item)),
           streamFromRangeInclusive    :: forall stream monad item . Streamable stream monad item =>
                                                                         persistedStream item ->
                                                                         Offset ->
                                                                         Offset ->
                                                                         stream monad (Either SomeException (Persisted item)),
           streamAllInfinitely :: forall stream monad item . Streamable stream monad item =>
                                                              persistedStream item ->
                                                              stream monad (Either SomeException (Persisted item)) ,
           streamAll ::           forall stream monad item . Streamable stream monad item =>
                                                              persistedStream item ->
                                                              stream monad (Either SomeException (Persisted item))}

data Querying persistedStream = Querying {
                             retrieveLast :: forall item . FromJSON item =>
                                                              persistedStream item ->
                                                              IO( Either SomeException (Maybe (Persisted item))),
                             isStreamNotFound :: forall item . persistedStream item -> IO (Either SomeException Bool)}

data Subscribing persistedStream = Subscribing {
                              subscribe :: forall stream monad item . Streamable stream monad item =>
                                                                          persistedStream item ->
                                                                          stream monad (Either SomeException (Persisted item)),
                              subscribeOnOffset :: forall  item. FromJSON item =>
                                                                  persistedStream item ->
                                                                  Offset ->
                                                                  IO (Either SomeException (Persisted item)) }



