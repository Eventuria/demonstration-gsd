{-# LANGUAGE NamedFieldPuns #-}
module CQRS.Write.CommandConsumption.ConsumeACommand (getConsumeACommandForAnAggregate) where

import Eventuria.Commons.Logger.Core
import PersistedStreamEngine.Interface.PersistedItem
import CQRS.Write.Aggregate.Commands.Command
import CQRS.Write.StreamRepository
import PersistedStreamEngine.Interface.Read.Reading
import PersistedStreamEngine.Interface.Write.WDsl
import CQRS.Write.CommandConsumption.CommandHandler
import CQRS.Write.CommandConsumption.Core
import Data.Aeson
import CQRS.EDsl
import CQRS.Write.Aggregate.Commands.CommandHeader
import PersistedStreamEngine.Interface.Write.CqrsEDslToWDslTranslation (translate)
import Control.Monad.IO.Class (MonadIO(..))
import CQRS.Write.Serialization.ValidationState ()
import CQRS.Write.Aggregate.Ids.AggregateId


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