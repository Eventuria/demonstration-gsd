{-# LANGUAGE DuplicateRecordFields #-}

module Gsd.CommandHandler where

import Data.Set (fromList)

import Data.Maybe


import Data.Function ((&))
import Cqrs.EventStore.PersistedItem
import Cqrs.Aggregate.Commands.ValidationStates.ValidationState
import Cqrs.Aggregate.Commands.Command
import Gsd.Commands
import Gsd.Events
import Cqrs.CommandHandler
import Cqrs.EDsl
import Gsd.CommandPredicates

gsdCommandHandler :: CommandHandler
gsdCommandHandler persistedCommand@PersistedItem {offset = offset , item = command } snapshotMaybe
   | isAlreadyProcessed offset snapshotMaybe = SkipBecauseAlreadyProcessed
   | (isFirstCommand snapshotMaybe) && (isCreateWorkspaceCommand command) = Transact $ (fromJust $ fromCommand (command::Command)) & (\CreateWorkspace {commandId = commandId, workspaceId = workspaceId} -> do
        now <- getCurrentTime
        eventId <- getNewEventID
        persistEvent $ toEvent $ WorkspaceCreated {  eventId = eventId , createdOn = now, workspaceId = workspaceId}
        updateValidationState ValidationState {lastOffsetConsumed = 0 ,
                                                            commandsProcessed = fromList [commandId] ,
                                                            state = AggregateState { aggregateId = workspaceId }})
   | (not $ isFirstCommand snapshotMaybe ) && (isCreateWorkspaceCommand command) = Reject "first command muste be CreateWorkspace "
   | otherwise = Reject "scenario not handle yet"







