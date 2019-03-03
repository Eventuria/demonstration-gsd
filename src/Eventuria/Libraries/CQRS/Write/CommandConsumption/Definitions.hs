module Eventuria.Libraries.CQRS.Write.CommandConsumption.Definitions where

import Control.Exception

import Streamly (SerialT)

import Eventuria.Commons.Logger.Core

import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import Eventuria.Libraries.PersistedStreamEngine.Interface.Read.Reading
import Eventuria.Libraries.PersistedStreamEngine.Interface.Write.TransactionDSL

import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId
import Eventuria.Libraries.CQRS.Write.StreamRepository
import Eventuria.Libraries.CQRS.Write.CommandConsumption.Handling.CommandHandler

type ConsumeACommand          = Persisted Command     ->  IO (Either SomeException ())
type ConsumeAnAggregateStream = Persisted AggregateId ->  SerialT IO (Either SomeException ())
type ConsumeAnAggregate       = Persisted AggregateId ->  IO (Either SomeException ())
type TransactionInterpreter applicationState a = Persisted Command -> Transaction applicationState a -> IO (Either SomeException a)

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