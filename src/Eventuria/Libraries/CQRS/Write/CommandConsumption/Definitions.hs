module Eventuria.Libraries.CQRS.Write.CommandConsumption.Definitions where

import Control.Exception

import Streamly (SerialT)

import Eventuria.Commons.Logger.Core

import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import Eventuria.Libraries.PersistedStreamEngine.Interface.Read.Reading
import Eventuria.Libraries.PersistedStreamEngine.Interface.Write.Writing

import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId
import Eventuria.Libraries.CQRS.Write.StreamRepository

import Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandHandlingResult

type ProjectWriteModel writeModel = Maybe writeModel -> CommandHandlingResult -> Maybe writeModel

type HandleCommand writeModel = Maybe writeModel -> (Persisted Command) -> IO (CommandHandlingResult)

type OrchestratreCommandConsumptionForAggregate writeModel = Persisted AggregateId ->  SerialT IO (Either SomeException (Maybe writeModel))

type GetAggregateCommandConsumptionOrchestration persistedStream writeModel =
       Logger ->
       GetCommandStream persistedStream ->
       GetCommandTransactionStream persistedStream  ->
       Reading persistedStream ->
       Writing persistedStream ->
       ProjectWriteModel writeModel ->
       HandleCommand writeModel ->
       OrchestratreCommandConsumptionForAggregate writeModel

