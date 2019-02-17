{-# LANGUAGE TypeSynonymInstances #-}
module CQRS.Write.Serialization.AggregateId where

import PersistedStreamEngine.Interface.Write.Writable
import CQRS.Write.Aggregate.Ids.AggregateId

instance Writable AggregateId where
  getItemName aggregateId  = "aggregateId"


