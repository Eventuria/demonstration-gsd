{-# LANGUAGE DuplicateRecordFields #-}

module Gsd.CommandHandler where

import Data.Set (fromList)
import Data.Maybe
import Data.Function ((&))

import Gsd.Commands
import Gsd.Events
import Gsd.CommandPredicates

import Cqrs.CommandHandler
import Cqrs.EDsl
import Cqrs.Aggregate.Commands.ValidationStates.ValidationState
import Cqrs.Aggregate.Commands.Command

import Cqrs.PersistedStream.PersistedItem

gsdCommandHandler :: CommandHandler
gsdCommandHandler persistedCommand@PersistedItem {offset = offset , item = command } snapshotMaybe
   | isAlreadyProcessed offset snapshotMaybe = SkipBecauseAlreadyProcessed
   | (isFirstCommand snapshotMaybe) && (isCreateWorkspaceCommand command) = Validate $ (fromJust $ fromCommand (command::Command)) & (\CreateWorkspace {commandId = commandId, workspaceId = workspaceId} -> do
        now <- getCurrentTime
        eventId <- getNewEventID
        persistEvent $ toEvent $ WorkspaceCreated {  eventId = eventId , createdOn = now, workspaceId = workspaceId}
        updateValidationState ValidationState {lastOffsetConsumed = 0 ,
                                                            commandsProcessed = fromList [commandId] ,
                                                            state = AggregateState { aggregateId = workspaceId }})
   | (not $ isFirstCommand snapshotMaybe ) && (isCreateWorkspaceCommand command) = Reject "CreateWorkspace can only be a the first command "
   | otherwise = Reject "scenario not handle yet"







