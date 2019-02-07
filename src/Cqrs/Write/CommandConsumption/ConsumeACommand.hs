{-# LANGUAGE NamedFieldPuns #-}
module Cqrs.Write.CommandConsumption.ConsumeACommand (getConsumeACommandForAnAggregate) where

import Logger.Core
import PersistedStreamEngine.Interface.PersistedItem
import Cqrs.Write.Aggregate.Commands.Command
import Cqrs.Write.StreamRepository
import PersistedStreamEngine.Interface.Read.Reading
import PersistedStreamEngine.Interface.Write.WDsl
import Cqrs.Write.CommandConsumption.CommandHandler
import Cqrs.Write.CommandConsumption.Core
import Data.Aeson
import Cqrs.EDsl
import Cqrs.Write.Aggregate.Commands.CommandHeader
import PersistedStreamEngine.Interface.Write.CqrsEDslToWDslTranslation (translate)
import Control.Monad.IO.Class (MonadIO(..))
import Cqrs.Write.Serialization.ValidationState ()
import Cqrs.Write.Aggregate.Ids.AggregateId

type GetConsumeACommand persistedStreamEngine applicationState = Logger ->
                          Querying persistedStreamEngine ->
                          GetValidationStateStream persistedStreamEngine applicationState ->
                          TransactionInterpreter applicationState () ->
                          CommandHandler applicationState ->
                          (AggregateId -> ConsumeACommand)

getConsumeACommandForAnAggregate :: (FromJSON applicationState, Show applicationState) =>
                                      Logger ->
                                      Querying persistedStreamEngine ->
                                      GetValidationStateStream persistedStreamEngine applicationState ->
                                      TransactionInterpreter applicationState () ->
                                      CommandHandler applicationState ->
                                      (AggregateId -> ConsumeACommand)
getConsumeACommandForAnAggregate logger querying getValidationStateStream transactionInterpreter commandHandler =
  (\aggregateId ->
    consumeACommand
      logger
      querying
      transactionInterpreter
      commandHandler
      (getValidationStateStream aggregateId))

consumeACommand :: (FromJSON applicationState, Show applicationState) =>
                      Logger ->
                      Querying persistedStreamEngine ->
                      TransactionInterpreter applicationState ()->
                      CommandHandler applicationState ->
                      ValidateStateStream persistedStreamEngine applicationState ->
                      ConsumeACommand
consumeACommand logger
                Querying {retrieveLast}
                transactionInterpreter
                commandHandler
                validationStateStream
                persistedCommand @PersistedItem {
                                item = Command { commandHeader = commandHeader@CommandHeader {commandId} }} = do
  response <- liftIO $ (fmap.fmap.fmap) item $ retrieveLast validationStateStream
  case response of
      Right lastValidationState -> do
          liftIO $ logInfo logger $ "feeding command handler > "
            ++ (show persistedCommand) ++ " > validationState : " ++ show lastValidationState
          transactionInterpreter $ case (commandHandler persistedCommand lastValidationState) of
                              Reject reason -> rejectCommandTransaction lastValidationState commandHeader reason
                              SkipBecauseAlreadyProcessed -> skipCommandTransaction commandHeader
                              Validate commandTransaction -> validateCommandTransaction
                                                              commandHeader
                                                              (translate $ commandTransaction)
      Left error -> return $ Left error