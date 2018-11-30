{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Cqrs.EventStore.Write.WDsl where

import Cqrs.Aggregate.Commands.CommandId
import Cqrs.Aggregate.Commands.Responses.CommandResponse
import Cqrs.Aggregate.Commands.ValidationStates.ValidationState
import Cqrs.Aggregate.Events.Event
import Cqrs.Aggregate.Events.EventId

import Data.Time
import Control.Monad.Free
import qualified Data.Set as Set
import Cqrs.Aggregate.Ids.AggregateId

data Directive a = PersistEvent Event a
                | PersistValidationState ValidationState a
                | PersistCommandResponse CommandResponse a
                | GetCurrentTime (UTCTime -> a )
                | GetNewEventId (EventId -> a) deriving (Functor)

type WriteEventStoreLanguage a = Free Directive a

persistEvent :: Event -> WriteEventStoreLanguage ()
persistEvent event = Free (PersistEvent event (Pure ()))

persistAggregate :: ValidationState -> WriteEventStoreLanguage ()
persistAggregate validationState = Free (PersistValidationState validationState (Pure ()))

persistCommandResponse :: CommandResponse -> WriteEventStoreLanguage ()
persistCommandResponse commandResponse = Free (PersistCommandResponse commandResponse (Pure ()))

getNewEventID :: WriteEventStoreLanguage EventId
getNewEventID = Free (GetNewEventId Pure)

getCurrentTime :: WriteEventStoreLanguage UTCTime
getCurrentTime = Free (GetCurrentTime Pure)

validateCommandTransaction :: AggregateId -> CommandId -> WriteEventStoreLanguage () -> WriteEventStoreLanguage ()
validateCommandTransaction  aggregateId commandId transaction  = do
    transaction
    persistCommandResponse CommandSuccessfullyProcessed {
                                   commandId = commandId  ,
                                   aggregateId = aggregateId }



skipCommandTransaction :: AggregateId -> CommandId -> WriteEventStoreLanguage ()
skipCommandTransaction  aggregateId commandId  = do
    persistCommandResponse CommandSkippedBecauseAlreadyProcessed {
                                   commandId = commandId  ,
                                   aggregateId = aggregateId }



rejectCommandTransaction :: Maybe ValidationState -> AggregateId -> CommandId -> RejectionReason -> WriteEventStoreLanguage ()
rejectCommandTransaction (Just snapshot) aggregateId commandId rejectionReason = do
    persistAggregate ValidationState { lastOffsetConsumed = (lastOffsetConsumed snapshot) + 1 ,
                                         commandsProcessed = Set.insert commandId (commandsProcessed snapshot),
                                         state = state snapshot}
    persistCommandResponse CommandFailed {
                                   commandId = commandId  ,
                                   aggregateId = aggregateId,
                                   reason = rejectionReason }
rejectCommandTransaction Nothing aggregateId commandId rejectionReason = do
    persistAggregate ValidationState { lastOffsetConsumed = 0 ,
                                         commandsProcessed = Set.fromList [commandId],
                                         state = AggregateState { aggregateId = aggregateId}}
    persistCommandResponse CommandFailed {
                                   commandId = commandId  ,
                                   aggregateId = aggregateId,
                                   reason = rejectionReason }