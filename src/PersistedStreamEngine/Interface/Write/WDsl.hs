{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module PersistedStreamEngine.Interface.Write.WDsl where

import CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import CQRS.Write.Aggregate.Events.Event
import CQRS.Write.Aggregate.Events.EventId

import Data.Time
import Control.Monad.Free
import qualified Data.Set as Set
import CQRS.Write.Aggregate.Commands.CommandHeader
import System.SafeResponse

type TransactionInterpreter applicationState a = WritePersistenceStreamLanguage applicationState a -> IO (SafeResponse a)

data Directive applicationState a = PersistEvent Event a
                | PersistValidationState (ValidationState applicationState) a
                | PersistCommandResponse CommandResponse a
                | GetCurrentTime (UTCTime -> a )
                | GetNewEventId (EventId -> a) deriving (Functor)

type WritePersistenceStreamLanguage applicationState a = Free (Directive applicationState) a

persistEvent :: Event -> WritePersistenceStreamLanguage applicationState ()
persistEvent event = Free (PersistEvent event (Pure ()))

persistAggregate :: ValidationState applicationState -> WritePersistenceStreamLanguage applicationState ()
persistAggregate validationState = Free (PersistValidationState validationState (Pure ()))

persistCommandResponse :: CommandResponse -> WritePersistenceStreamLanguage applicationState ()
persistCommandResponse commandResponse = Free (PersistCommandResponse commandResponse (Pure ()))

getNewEventID :: WritePersistenceStreamLanguage applicationState EventId
getNewEventID = Free (GetNewEventId Pure)

getCurrentTime :: WritePersistenceStreamLanguage applicationState UTCTime
getCurrentTime = Free (GetCurrentTime Pure)

validateCommandTransaction :: CommandHeader -> WritePersistenceStreamLanguage applicationState () -> WritePersistenceStreamLanguage applicationState ()
validateCommandTransaction  commandHeaderProcessed transaction  = do
    transaction
    persistCommandResponse CommandSuccessfullyProcessed {commandHeaderProcessed}



skipCommandTransaction :: CommandHeader -> WritePersistenceStreamLanguage applicationState ()
skipCommandTransaction  commandHeaderProcessed  = return ()

rejectCommandTransaction :: Maybe (ValidationState applicationState) -> CommandHeader -> RejectionReason -> WritePersistenceStreamLanguage applicationState ()
rejectCommandTransaction (Just snapshot) commandHeaderProcessed@CommandHeader{aggregateId, commandId} rejectionReason = do
    persistAggregate ValidationState {  lastOffsetConsumed = (lastOffsetConsumed snapshot) + 1 ,
                                        aggregateId,
                                        commandsProcessed = Set.insert commandId (commandsProcessed snapshot),
                                        state = state snapshot}
    persistCommandResponse CommandFailed {commandHeaderProcessed, reason = rejectionReason }
rejectCommandTransaction Nothing commandHeaderProcessed@CommandHeader{aggregateId, commandId}  rejectionReason = do
    persistAggregate ValidationState { lastOffsetConsumed = 0 ,
                                         commandsProcessed = Set.fromList [commandId],
                                         aggregateId,
                                         state = Nothing}
    persistCommandResponse CommandFailed {commandHeaderProcessed,reason = rejectionReason }