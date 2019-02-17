module CQRS.Write.Aggregate.Core where

import CQRS.Write.Aggregate.Ids.AggregateId

class AggregateJoinable item where {
  getAggregateId :: item ->  AggregateId
}


