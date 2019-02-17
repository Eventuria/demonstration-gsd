module CQRS.Write.CommandConsumption.Core where

import Logger.Core
import PersistedStreamEngine.Interface.PersistedItem
import CQRS.Write.Aggregate.Commands.Command
import CQRS.Write.Aggregate.Ids.AggregateId
import System.SafeResponse
import CQRS.Write.StreamRepository
import PersistedStreamEngine.Interface.Read.Reading
import Streamly (SerialT)
import PersistedStreamEngine.Interface.Write.WDsl
import CQRS.Write.CommandConsumption.CommandHandler

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