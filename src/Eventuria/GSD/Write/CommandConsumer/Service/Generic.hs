{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Eventuria.GSD.Write.CommandConsumer.Service.Generic  where

import           Control.Exception

import           Eventuria.Commons.Logger.Core

import           Eventuria.Libraries.PersistedStreamEngine.Interface.Write.WDsl
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Read.Reading

import qualified Eventuria.Libraries.CQRS.Write.CommandConsumption.Main as CQRS.Write.CommandConsumption
import           Eventuria.Libraries.CQRS.Write.StreamRepository
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.ConsumeAnAggregate (getConsumeAnAggregate)
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.ConsumeACommand (getConsumeACommandForAnAggregate)
import           Eventuria.Libraries.CQRS.Write.Serialization.CommandResponse()


import           Eventuria.GSD.Write.CommandConsumer.Handling.CommandHandler (commandHandler)
import           Eventuria.GSD.Write.Model.State



consumeCommands :: Logger ->
                           CqrsStreamRepository persistedStream GsdState ->
                           Reading persistedStream ->
                           TransactionInterpreter GsdState () ->
                           IO (Either SomeException ())
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
