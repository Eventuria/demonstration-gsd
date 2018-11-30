module Cqrs.Aggregate.Core where

import Cqrs.Aggregate.Ids.AggregateId

class AggregateJoinable item where {
  getAggregateId :: item ->  AggregateId
}


