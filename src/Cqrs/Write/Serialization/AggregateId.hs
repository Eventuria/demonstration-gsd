{-# LANGUAGE TypeSynonymInstances #-}
module Cqrs.Write.Serialization.AggregateId where

import PersistedStreamEngine.Write.Writable
import Cqrs.Write.Aggregate.Ids.AggregateId

instance Writable AggregateId where
  getItemName aggregateId  = "aggregateId"


