module Eventuria.Libraries.CQRS.Write.CommandConsumption.Definitions where

import Control.Exception

import Streamly (SerialT)

import Eventuria.Commons.Logger.Core

import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import Eventuria.Libraries.PersistedStreamEngine.Interface.Read.Reading
import Eventuria.Libraries.PersistedStreamEngine.Interface.Write.Writing
import Eventuria.Libraries.PersistedStreamEngine.Interface.Write.PersistenceResult

import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId
import Eventuria.Libraries.CQRS.Write.StreamRepository
import Eventuria.Libraries.CQRS.Write.CommandConsumption.Handling.CommandHandler

type ConsumeACommand          = Persisted Command     ->  IO (Either SomeException PersistenceResult)
type ConsumeAnAggregateStream = Persisted AggregateId ->  SerialT IO (Either SomeException ())
type ConsumeAnAggregate       = Persisted AggregateId ->  IO (Either SomeException ())


type GetConsumeAnAggregate persistedStream writeModel =
       Logger ->
       GetCommandStream persistedStream ->
       GetCommandTransactionStream persistedStream writeModel ->
       Reading persistedStream ->
       Writing persistedStream ->
       CommandHandler writeModel ->
       GetConsumeACommand persistedStream writeModel ->
       ConsumeAnAggregate

type GetConsumeACommand persistedStream writeModel = Logger ->
                          Querying persistedStream ->
                          Writing persistedStream ->
                          GetCommandTransactionStream persistedStream writeModel ->
                          CommandHandler writeModel ->
                          (AggregateId -> ConsumeACommand)