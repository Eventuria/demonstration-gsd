{-# LANGUAGE TypeSynonymInstances #-}
module Cqrs.Aggregate.Ids.AggregateId where

import Data.UUID
import EventStore.Write.Persisting

type AggregateId = UUID

instance Writable AggregateId where
  getItemName aggregateId  = "aggregateId"

