module Cqrs.Write.CommandConsumption.Core where


import PersistedStreamEngine.Interface.PersistedItem
import Cqrs.Write.Aggregate.Commands.Command
import Cqrs.Write.Aggregate.Ids.AggregateId
import System.SafeResponse
import Streamly (SerialT)

type ConsumeACommand          = Persisted Command     ->  IO (SafeResponse ())
type ConsumeAnAggregateStream = Persisted AggregateId ->  SerialT IO (SafeResponse ())
type ConsumeAnAggregate       = Persisted AggregateId ->  IO (SafeResponse ())



