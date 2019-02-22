{-# LANGUAGE TypeSynonymInstances #-}
module Eventuria.Libraries.CQRS.Write.Serialization.AggregateId where

import Eventuria.Libraries.PersistedStreamEngine.Interface.Write.Writable
import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId

instance Writable AggregateId where
  getItemName aggregateId  = "aggregateId"


