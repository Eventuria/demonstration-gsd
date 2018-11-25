module Cqrs.Aggregate.Ids.PersistedAggregateId where

import Cqrs.Streams
import Cqrs.Aggregate.Ids.AggregateId

data PersistedAggregateId = PersistedAggregateId { offset :: Offset, persistedAggregateId :: AggregateId } deriving (Show,Eq)

