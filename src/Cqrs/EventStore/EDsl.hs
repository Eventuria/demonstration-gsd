{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Cqrs.EventStore.EDsl where

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

type EventStoreLanguage a = Free Directive a

persistEvent :: Event -> EventStoreLanguage ()
persistEvent event = Free (PersistEvent event (Pure ()))

persistAggregate :: ValidationState -> EventStoreLanguage ()
persistAggregate validationState = Free (PersistValidationState validationState (Pure ()))

persistCommandResponse :: CommandResponse -> EventStoreLanguage ()
persistCommandResponse commandResponse = Free (PersistCommandResponse commandResponse (Pure ()))

getNewEventID :: EventStoreLanguage EventId
getNewEventID = Free (GetNewEventId Pure)

getCurrentTime :: EventStoreLanguage UTCTime
getCurrentTime = Free (GetCurrentTime Pure)

validateCommandTransaction :: AggregateId -> CommandId -> EventStoreLanguage () -> EventStoreLanguage ()
validateCommandTransaction  aggregateId commandId transaction  = do
    transaction
    persistCommandResponse CommandSuccessfullyProcessed {
                                   commandId = commandId  ,
                                   aggregateId = aggregateId }



skipCommandTransaction :: AggregateId -> CommandId -> EventStoreLanguage ()
skipCommandTransaction  aggregateId commandId  = do
    persistCommandResponse CommandSkippedBecauseAlreadyProcessed {
                                   commandId = commandId  ,
                                   aggregateId = aggregateId }



rejectCommandTransaction :: Maybe ValidationState -> AggregateId -> CommandId -> RejectionReason -> EventStoreLanguage ()
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