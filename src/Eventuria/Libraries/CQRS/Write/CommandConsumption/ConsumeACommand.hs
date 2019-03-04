{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Eventuria.Libraries.CQRS.Write.CommandConsumption.ConsumeACommand (getConsumeACommandForAnAggregate) where

import Data.Aeson

import Eventuria.Commons.Logger.Core

import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import Eventuria.Libraries.PersistedStreamEngine.Interface.Read.Reading
import Eventuria.Libraries.PersistedStreamEngine.Interface.Write.Writing

import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import Eventuria.Libraries.CQRS.Write.StreamRepository
import Eventuria.Libraries.CQRS.Write.CommandConsumption.Handling.CommandHandler
import Eventuria.Libraries.CQRS.Write.CommandConsumption.Definitions
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandHeader
import Eventuria.Libraries.CQRS.Write.CommandConsumption.Transaction.CommandTransaction
import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId
import Eventuria.Libraries.CQRS.Write.Serialization.CommandTransaction ()


getConsumeACommandForAnAggregate :: (FromJSON writeModel, ToJSON writeModel,Show writeModel) =>
                                      Logger ->
                                      Querying persistedStream ->
                                      Writing persistedStream ->
                                      GetCommandTransactionStream persistedStream writeModel ->
                                      CommandHandler writeModel ->
                                      (AggregateId -> ConsumeACommand)
getConsumeACommandForAnAggregate logger querying writing getCommandTransactionStream commandHandler =
  (\aggregateId ->
    consumeACommand
      logger
      querying
      writing
      commandHandler
      (getCommandTransactionStream aggregateId))

consumeACommand :: (FromJSON writeModel, ToJSON writeModel, Show writeModel) =>
                      Logger ->
                      Querying persistedStream ->
                      Writing persistedStream ->
                      CommandHandler writeModel ->
                      CommandTransactionStream persistedStream writeModel ->
                      ConsumeACommand
consumeACommand logger
                Querying {retrieveLast}
                Writing {persist}
                commandHandler
                commandTransactionStream
                persistedCommand @ PersistedItem {
                                    item = Command { commandHeader = commandHeader@CommandHeader {commandId} }} = do
  response <- (fmap.fmap.fmap) item $ retrieveLast commandTransactionStream
  case response of
      Right (Nothing) -> do
          logInfo logger $ "[consume.command] consuming command " ++ (show persistedCommand)
          logInfo logger $ "[consume.command] No write model for this command"
          commandHandlingResponse <- commandHandler
                                        Nothing
                                        persistedCommand
          logInfo logger $ "[consume.command] commandHandlingResponse : " ++ show commandHandlingResponse
          persist commandTransactionStream $ toCommandTransaction
                                                persistedCommand
                                                commandHandlingResponse
      Right (Just (CommandTransaction { snapshot = Snapshot {offset,writeModelMaybe}}))  -> do
          logInfo logger $ "[consume.command] consuming command " ++ (show persistedCommand)
          logInfo logger $ "[consume.command] last write model state " ++ show writeModelMaybe
          commandHandlingResponse <- commandHandler
                                        writeModelMaybe
                                        persistedCommand
          logInfo logger $ "[consume.command] commandHandlingResponse : " ++ show commandHandlingResponse
          persist commandTransactionStream $ toCommandTransaction
                                                persistedCommand
                                                commandHandlingResponse
      Left error -> return $ Left error