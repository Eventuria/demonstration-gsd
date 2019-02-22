module Eventuria.Libraries.CQRS.Write.CommandConsumption.Core where

import Eventuria.Commons.Logger.Core
import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId
import Eventuria.Commons.System.SafeResponse
import Eventuria.Libraries.CQRS.Write.StreamRepository
import Eventuria.Libraries.PersistedStreamEngine.Interface.Read.Reading
import Streamly (SerialT)
import Eventuria.Libraries.PersistedStreamEngine.Interface.Write.WDsl
import Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandHandler

type ConsumeACommand          = Persisted Command     ->  IO (SafeResponse ())
type ConsumeAnAggregateStream = Persisted AggregateId ->  SerialT IO (SafeResponse ())
type ConsumeAnAggregate       = Persisted AggregateId ->  IO (SafeResponse ())

type GetConsumeAnAggregate persistedStreamEngine applicationState =
       Logger ->
       GetCommandStream persistedStreamEngine ->
       GetValidationStateStream persistedStreamEngine applicationState ->
       Reading persistedStreamEngine ->
       TransactionInterpreter applicationState () ->
       CommandHandler applicationState ->
       GetConsumeACommand persistedStreamEngine applicationState ->
       ConsumeAnAggregate

type GetConsumeACommand persistedStreamEngine applicationState = Logger ->
                          Querying persistedStreamEngine ->
                          GetValidationStateStream persistedStreamEngine applicationState ->
                          TransactionInterpreter applicationState () ->
                          CommandHandler applicationState ->
                          (AggregateId -> ConsumeACommand)