{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Gsd.Write.Flow.CommandConsumer.Service.Generic  where

import Logger.Core

import qualified CQRS.Write.CommandConsumption.Main as CQRS.Write.CommandConsumption
import CQRS.Write.StreamRepository

import PersistedStreamEngine.Interface.Read.Reading
import PersistedStreamEngine.Interface.Write.WDsl
import CQRS.Write.CommandConsumption.ConsumeAnAggregate (getConsumeAnAggregate)
import CQRS.Write.CommandConsumption.ConsumeACommand (getConsumeACommandForAnAggregate)
import Gsd.Write.Flow.CommandConsumer.Handling.CommandHandler (commandHandler)
import Gsd.Write.Model.State
import CQRS.Write.Serialization.CommandResponse()
import System.SafeResponse


consumeCommands :: Logger ->
                           CqrsStreamRepository persistedStream GsdState ->
                           Reading persistedStream ->
                           TransactionInterpreter GsdState () ->
                           IO (SafeResponse ())
consumeCommands logger
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
