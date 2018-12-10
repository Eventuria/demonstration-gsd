module Cqrs.Write.Aggregate.Core where

import Cqrs.Write.Aggregate.Ids.AggregateId

class AggregateJoinable item where {
  getAggregateId :: item ->  AggregateId
}


