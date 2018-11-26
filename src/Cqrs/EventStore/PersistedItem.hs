module Cqrs.EventStore.PersistedItem where

import Cqrs.Streams

data Persisted item = PersistedItem {
                                offset :: Offset ,
                                item :: item} deriving Show
