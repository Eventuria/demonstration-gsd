{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Eventuria.GSD.Write.Flow.CommandConsumer.Service.Generic  where

import Eventuria.Libraries.CQRS.Write.StreamRepository
import Eventuria.Libraries.CQRS.Write.CommandConsumption.ConsumeAnAggregate (getConsumeAnAggregate)
import Eventuria.Libraries.CQRS.Write.CommandConsumption.ConsumeACommand (getConsumeACommandForAnAggregate)
import Eventuria.Libraries.CQRS.Write.Serialization.CommandResponse()
import qualified Eventuria.Libraries.CQRS.Write.CommandConsumption.Main as CQRS.Write.CommandConsumption

import Eventuria.Libraries.PersistedStreamEngine.Interface.Write.WDsl
import Eventuria.Libraries.PersistedStreamEngine.Interface.Read.Reading

import Eventuria.GSD.Write.Flow.CommandConsumer.Handling.CommandHandler (commandHandler)
import Eventuria.GSD.Write.Model.State

import Eventuria.Commons.Logger.Core
import Eventuria.Commons.System.SafeResponse

consumeCommands :: Logger ->
                           CqrsStreamRepository persistedStream GsdState ->
                           Reading persistedStream ->
                           TransactionInterpreter GsdState () ->
                           IO (SafeResponse ())
consumeCommands logger cqrsStreamRepository @ CqrsStreamRepository {
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
