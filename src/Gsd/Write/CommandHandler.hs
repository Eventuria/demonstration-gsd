{-# LANGUAGE DuplicateRecordFields #-}

module Gsd.Write.CommandHandler where

import Data.Set (fromList)
import Data.Maybe
import Data.Function ((&))

import Gsd.Write.Commands
import Gsd.Write.Events
import Gsd.Write.CommandPredicates

import Cqrs.Write.CommandConsumption.CommandHandler
import Cqrs.EDsl
import Cqrs.Write.Aggregate.Commands.ValidationStates.ValidationState
import Cqrs.Write.Aggregate.Commands.Command

import PersistedStreamEngine.Interface.PersistedItem

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







