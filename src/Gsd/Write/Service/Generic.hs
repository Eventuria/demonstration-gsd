{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Gsd.Write.Service.Generic  where

import Logger.Core

import qualified CQRS.Write.CommandConsumption.Main as CQRS.Write.CommandConsumption
import qualified CQRS.Write.CqrsWrite as CQRS.Write
import CQRS.Write.StreamRepository

import PersistedStreamEngine.Interface.Write.Writing
import PersistedStreamEngine.Interface.Read.Reading
import PersistedStreamEngine.Interface.Write.WDsl
import CQRS.Write.CommandConsumption.ConsumeAnAggregate (getConsumeAnAggregate)
import CQRS.Write.CommandConsumption.ConsumeACommand (getConsumeACommandForAnAggregate)
import Gsd.Write.Command.Handling.CommandHandler (commandHandler)
import Gsd.Write.Model.Commands.Command
import Gsd.Write.Model.State
import CQRS.Write.PersistCommandResult
import CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import PersistedStreamEngine.Interface.PersistedItem
import CQRS.Write.Aggregate.Ids.AggregateId
import PersistedStreamEngine.Interface.Offset
import CQRS.Write.Aggregate.Commands.CommandId
import CQRS.Write.Serialization.CommandResponse()
import System.SafeResponse

persistCommand ::  AggregateIdStream persistedStream ->
                   GetCommandStream persistedStream->
                   Querying persistedStream ->
                   Writing persistedStream ->
                   GsdCommand ->
                   IO PersistCommandResult
persistCommand aggregateIdStream getCommandStream querying writing gsdCommand =
  CQRS.Write.persistCommand
    writing
    querying
    getCommandStream
    aggregateIdStream $ toCommand gsdCommand

startCommandConsumption :: Logger ->
                           CqrsStreamRepository persistedStream GsdState ->
                           Reading persistedStream ->
                           TransactionInterpreter GsdState () ->
                           IO (SafeResponse ())
startCommandConsumption logger
                        cqrsStreamRepository @ CqrsStreamRepository {
                                                  aggregateIdStream,
                                                  getCommandStream,
                                                  getValidationStateStream}
                        reading @ Reading { streaming ,querying}
                        transactionInterpreter   =
   CQRS.Write.CommandConsumption.execute
      logger
      aggregateIdStream
      streaming
      (getConsumeAnAggregate
        logger
        getCommandStream
        getValidationStateStream
        reading
        transactionInterpreter
        commandHandler
        getConsumeACommandForAnAggregate)




waitTillCommandResponseProduced ::
                     GetCommandResponseStream persistedStream ->
                     Subscribing persistedStream ->
                     AggregateId ->
                     Offset ->
                     CommandId ->
                     IO (SafeResponse (Persisted CommandResponse))
waitTillCommandResponseProduced getCommandResponseStream subscribing @ Subscribing {subscribeOnOffset} aggregateId offset commandId =
    (subscribeOnOffset (getCommandResponseStream aggregateId) offset)




