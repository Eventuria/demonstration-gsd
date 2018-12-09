{-# LANGUAGE TypeSynonymInstances #-}
module Cqrs.Serialization.Aggregate.AggregateId where

import Cqrs.PersistedStreamEngine.Write.Writable
import Cqrs.Aggregate.Ids.AggregateId

instance Writable AggregateId where
  getItemName aggregateId  = "aggregateId"


