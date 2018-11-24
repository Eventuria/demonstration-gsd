{-# LANGUAGE DuplicateRecordFields #-}

module Gsd.CommandHandler where

import Data.Set (fromList)

import Data.Maybe


import Data.Function ((&))

import Cqrs.Aggregate.Snapshots.AggregateSnapshot
import Cqrs.Commands.PersistedCommand
import Cqrs.Commands.Command
import Gsd.Commands
import Gsd.Events
import Cqrs.CommandHandler
import Cqrs.EDsl
import Gsd.CommandPredicates

gsdCommandHandler :: CommandHandler
gsdCommandHandler persistedCommand@PersistedCommand {offset = offset , command = command } snapshotMaybe
   | isAlreadyProcessed offset snapshotMaybe = SkipBecauseAlreadyProcessed
   | (isFirstCommand snapshotMaybe) && (isCreateWorkspaceCommand command) = Transact $ (fromJust $ fromCommand (command::Command)) & (\CreateWorkspace {commandId = commandId, workspaceId = workspaceId} -> do
        now <- getCurrentTime
        eventId <- getNewEventID
        persistEvent $ toEvent $ WorkspaceCreated {  eventId = eventId , createdOn = now, workspaceId = workspaceId}
        updateSnapshot AggregateSnapshot {lastOffsetConsumed = 0 ,
                                                            commandsProcessed = fromList [commandId] ,
                                                            state = AggregateState { aggregateId = workspaceId }})
   | (not $ isFirstCommand snapshotMaybe ) && (isCreateWorkspaceCommand command) = Reject "first command muste be CreateWorkspace "
   | otherwise = Reject "scenario not handle yet"







