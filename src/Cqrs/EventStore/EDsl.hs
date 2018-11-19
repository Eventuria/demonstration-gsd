{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Cqrs.EventStore.EDsl where

import Cqrs.CommandResponse
import Cqrs.PersistedCommand
import Cqrs.Snapshot
import Cqrs.Events
import Cqrs.Core
import Data.Time
import Control.Monad.Free
import qualified Data.Set as Set


data Directive a = PersistEvent Event a
                | PersistAggregate AggregateSnapshot a
                | PersistCommandResponse CommandResponse a
                | GetCurrentTime (UTCTime -> a )
                | GetNewEventId (EventId -> a) deriving (Functor)

type EventStoreLanguage a = Free Directive a

persistEvent :: Event -> EventStoreLanguage ()
persistEvent event = Free (PersistEvent event (Pure ()))

persistAggregate :: AggregateSnapshot -> EventStoreLanguage ()
persistAggregate aggregateSnapshot = Free (PersistAggregate aggregateSnapshot (Pure ()))

persistCommandResponse :: CommandResponse -> EventStoreLanguage ()
persistCommandResponse commandResponse = Free (PersistCommandResponse commandResponse (Pure ()))

getNewEventID :: EventStoreLanguage EventId
getNewEventID = Free (GetNewEventId Pure)

getCurrentTime :: EventStoreLanguage UTCTime
getCurrentTime = Free (GetCurrentTime Pure)

skipCommandTransaction :: AggregateId -> CommandId -> EventStoreLanguage ()
skipCommandTransaction  aggregateId commandId  = do
    persistCommandResponse CommandSkippedBecauseAlreadyProcessed {
                                   commandId = commandId  ,
                                   aggregateId = aggregateId }


rejectCommandTransaction :: Maybe AggregateSnapshot -> AggregateId -> CommandId -> RejectionReason -> EventStoreLanguage ()
rejectCommandTransaction (Just snapshot) aggregateId commandId rejectionReason = do
    persistAggregate AggregateSnapshot { lastOffsetConsumed = (lastOffsetConsumed snapshot) + 1 ,
                                         commandsProcessed = Set.insert commandId (commandsProcessed snapshot),
                                         state = state snapshot}
    persistCommandResponse CommandFailed {
                                   commandId = commandId  ,
                                   aggregateId = aggregateId,
                                   reason = rejectionReason }
rejectCommandTransaction Nothing aggregateId commandId rejectionReason = do
    persistAggregate AggregateSnapshot { lastOffsetConsumed = 0 ,
                                         commandsProcessed = Set.fromList [commandId],
                                         state = AggregateState { aggregateId = aggregateId}}
    persistCommandResponse CommandFailed {
                                   commandId = commandId  ,
                                   aggregateId = aggregateId,
                                   reason = rejectionReason }