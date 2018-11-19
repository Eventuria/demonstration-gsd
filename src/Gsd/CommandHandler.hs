{-# LANGUAGE DuplicateRecordFields #-}

module Gsd.CommandHandler where

import Data.Set (Set)
import qualified Data.Set as Set


import Data.Maybe
import Data.Either

import Data.Function ((&))

import Cqrs.Logger
import Cqrs.PersistedCommand
import Cqrs.Snapshot
import Cqrs.Command
import Cqrs.Events
import Cqrs.Streams
import Gsd.Commands
import Cqrs.CommandResponse
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
                                                            commandsProcessed = Set.fromList [commandId] ,
                                                            state = AggregateState { aggregateId = workspaceId }})
   | (not $ isFirstCommand snapshotMaybe ) && (isCreateWorkspaceCommand command) = Reject "first command muste be CreateWorkspace "
   | otherwise = Reject "scenario not handle yet"







