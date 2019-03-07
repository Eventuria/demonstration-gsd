{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Eventuria.GSD.Write.CommandConsumer.Service.Generic  where

import           Control.Exception

import           Eventuria.Commons.Logger.Core

import           Eventuria.Libraries.PersistedStreamEngine.Interface.Write.Writing
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Read.Reading

import qualified Eventuria.Libraries.CQRS.Write.CommandConsumption.Service as CQRS.Write.Service
import           Eventuria.Libraries.CQRS.Write.StreamRepository
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.ConsumeAnAggregate (getConsumeAnAggregate)
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.ConsumeACommand (getConsumeACommandForAnAggregate)
import           Eventuria.Libraries.CQRS.Write.Serialization.CommandResponse()


import           Eventuria.GSD.Write.CommandConsumer.Handling.CommandHandler (commandHandler)
import           Eventuria.GSD.Write.Model.WriteModel



consumeCommands :: Logger ->
                   CQRSWriteStreamRepository persistedStream GsdWriteModel ->
                   Reading persistedStream ->
                   Writing persistedStream ->
                   IO (Either SomeException ())
consumeCommands logger cqrsStreamRepository @ CQRSWriteStreamRepository {
                                                  aggregateIdStream,
                                                  getCommandStream,
                                                  getCommandTransactionStream}
                       reading @ Reading { streaming ,querying}
                       writing =
   CQRS.Write.Service.startCommandConsumption
      logger
      aggregateIdStream
      streaming
      (getConsumeAnAggregate
        logger
        getCommandStream
        getCommandTransactionStream
        reading
        writing
        commandHandler
        getConsumeACommandForAnAggregate)
