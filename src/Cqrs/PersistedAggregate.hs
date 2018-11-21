module Cqrs.PersistedAggregate where

import Cqrs.Core
import Cqrs.Streams

data PersistedAggregate = PersistedAggregate { offset :: Offset, aggregateIdPersisted :: AggregateId } deriving (Show,Eq)
