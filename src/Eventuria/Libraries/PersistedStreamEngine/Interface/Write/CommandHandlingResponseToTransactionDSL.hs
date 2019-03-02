{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.Libraries.PersistedStreamEngine.Interface.Write.CommandHandlingResponseToTransactionDSL where

import Control.Monad.Free

import qualified Data.Set as Set


import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import qualified Eventuria.Libraries.PersistedStreamEngine.Interface.Write.TransactionDSL as TransactionDSL
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Write.TransactionDSL

import qualified Eventuria.Libraries.CQRS.Write.CommandConsumption.Handling.ResponseDSL   as ResponseDSL
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.Handling.ResponseDSL

import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandHeader
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse

translateIntoTransaction :: Maybe (ValidationState applicationState) ->
             Persisted Command ->
             CommandHandlingResponse applicationState  -> Transaction applicationState ()

translateIntoTransaction
       snapshotMaybe
       persistedCommand @ PersistedItem { item = Command { commandHeader = commandHeaderProcessed@CommandHeader {..}}}
       (RejectCommand rejectionReason)  =

  case snapshotMaybe of
    Just snapshot ->
      transactionStart persistedCommand >>
      persistAggregate ValidationState {  lastOffsetConsumed = (lastOffsetConsumed snapshot) + 1 ,
                                          commandsProcessed = Set.insert commandId (commandsProcessed snapshot),
                                          state = state snapshot, ..} >>
      persistCommandResponse CommandFailed {commandHeaderProcessed, reason = rejectionReason } >>
      transactionEnd persistedCommand
    Nothing ->
      transactionStart persistedCommand >>
      persistAggregate ValidationState { lastOffsetConsumed = 0 ,
                                               commandsProcessed = Set.fromList [commandId],
                                               aggregateId,
                                               state = Nothing} >>
      persistCommandResponse CommandFailed {commandHeaderProcessed,reason = rejectionReason } >>
      transactionEnd persistedCommand

translateIntoTransaction
       snapshotMaybe
       persistedCommand @ PersistedItem { item = Command { commandHeader = commandHeaderProcessed@CommandHeader {..}}}
       (ValidateCommandWithFollowingTransactionPayload transactionPayload)  =
  transactionStart persistedCommand >>
  translateTransactionPayload transactionPayload >>
  persistCommandResponse CommandSuccessfullyProcessed {commandHeaderProcessed} >>
  transactionEnd persistedCommand

translateIntoTransaction
       snapshotMaybe
       persistedCommand
       SkipCommandBecauseAlreadyProcessed  = return ()


translateTransactionPayload :: TransactionPayload applicationState ()  -> Transaction applicationState ()
translateTransactionPayload (Pure a)  = return a
translateTransactionPayload (Free (ResponseDSL.PersistEvent event next))                     = Free (TransactionDSL.PersistEvent           event $ translateTransactionPayload next)
translateTransactionPayload (Free (ResponseDSL.UpdateValidationState validationState next))  = Free (TransactionDSL.PersistValidationState validationState $ translateTransactionPayload next)
translateTransactionPayload (Free (ResponseDSL.GetCurrentTime fct))                          = Free (TransactionDSL.GetCurrentTime       $ fmap translateTransactionPayload fct)
translateTransactionPayload (Free (ResponseDSL.GetNewEventId fct))                           = Free (TransactionDSL.GetNewEventId        $ fmap translateTransactionPayload fct)