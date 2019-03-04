{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}

module Eventuria.Libraries.CQRS.Read.StreamRepository where

import Control.Exception
import Streamly hiding (Streaming)

import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId


type GetStreamAll item = (AggregateId -> SerialT IO (Either SomeException(Persisted item)))

type StreamAll item = SerialT IO (Either SomeException(Persisted item))


