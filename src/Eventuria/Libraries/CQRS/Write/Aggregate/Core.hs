module Eventuria.Libraries.CQRS.Write.Aggregate.Core where

import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId

class AggregateJoinable item where {
  getAggregateId :: item ->  AggregateId
}


