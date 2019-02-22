{-# LANGUAGE NamedFieldPuns #-}
module Eventuria.Libraries.CQRS.Write.CommandConsumption.ConsumeACommand (getConsumeACommandForAnAggregate) where

import Eventuria.Commons.Logger.Core
import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import Eventuria.Libraries.CQRS.Write.StreamRepository
import Eventuria.Libraries.PersistedStreamEngine.Interface.Read.Reading
import Eventuria.Libraries.PersistedStreamEngine.Interface.Write.WDsl
import Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandHandler
import Eventuria.Libraries.CQRS.Write.CommandConsumption.Core
import Data.Aeson
import Eventuria.Libraries.CQRS.EDsl
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandHeader
import Eventuria.Libraries.PersistedStreamEngine.Interface.Write.CqrsEDslToWDslTranslation (translate)
import Control.Monad.IO.Class (MonadIO(..))
import Eventuria.Libraries.CQRS.Write.Serialization.ValidationState ()
import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId


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