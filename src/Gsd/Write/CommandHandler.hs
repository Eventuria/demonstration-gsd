{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Gsd.Write.CommandHandler where

import Data.Set (fromList)
import Data.Function ((&))

import Gsd.Write.Commands.Command
import Gsd.Write.Events.Event
import Gsd.Write.CommandPredicates

import Cqrs.Write.CommandConsumption.CommandHandler
import Cqrs.EDsl
import Cqrs.Write.Aggregate.Commands.ValidationStates.ValidationState
import Cqrs.Write.Aggregate.Commands.Command

import PersistedStreamEngine.Interface.PersistedItem

gsdCommandHandler :: CommandHandler
gsdCommandHandler persistedCommand@PersistedItem {offset = offset , item = command } snapshotMaybe
   | isAlreadyProcessed offset snapshotMaybe = SkipBecauseAlreadyProcessed
   | (isFirstCommand snapshotMaybe) && (isCreateWorkspaceCommand command) = Validate $ (fromCommand (command::Command)) & (\CreateWorkspace {commandId, workspaceId, workspaceName} -> do
        now <- getCurrentTime
        eventId <- getNewEventID
        persistEvent $ toEvent $ WorkspaceCreated {  eventId , createdOn = now, workspaceId}
        persistEvent $ toEvent $ WorkspaceNamed   {  eventId , createdOn = now, workspaceId , workspaceName}
        updateValidationState ValidationState {lastOffsetConsumed = 0 ,
                                                            commandsProcessed = fromList [commandId] ,
                                                            state = AggregateState { aggregateId = workspaceId }})
   | (isNotFirstCommand snapshotMaybe) && (isRenameWorkspaceCommand command) = Validate $ (fromCommand (command::Command)) & (\RenameWorkspace {commandId, workspaceId, workspaceNewName} -> do
           now <- getCurrentTime
           eventId <- getNewEventID
           persistEvent $ toEvent $ WorkspaceRenamed   {  eventId , createdOn = now, workspaceId , workspaceNewName}
           updateValidationState ValidationState {lastOffsetConsumed = 0 ,
                                                               commandsProcessed = fromList [commandId] ,
                                                               state = AggregateState { aggregateId = workspaceId }})
   | (isNotFirstCommand snapshotMaybe ) && (isCreateWorkspaceCommand command) = Reject "CreateWorkspace can only be a the first command "
   | otherwise = Reject "scenario not handle yet"







