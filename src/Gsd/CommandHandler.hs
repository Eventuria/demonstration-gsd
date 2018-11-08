{-# LANGUAGE DuplicateRecordFields #-}

module Gsd.CommandHandler where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.UUID.V4 as Uuid
import Data.Time
import Data.Maybe

import Streamly
import qualified Streamly.Prelude as S
import Control.Concurrent
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function ((&))

import qualified Database.EventStore as EventStore
import Cqrs.Logger
import Cqrs.PersistedCommand
import Cqrs.Snapshot
import qualified Cqrs.CommandResponseStream as CommandResponseStream
import qualified Cqrs.SnapshotStream as SnapshotStream
import qualified Cqrs.EventStream as EventStream
import Cqrs.Command
import Cqrs.Events
import Cqrs.Streams
import Gsd.Commands
import Cqrs.CommandResponse
import Gsd.Events
import Cqrs.CommandHandler
import Gsd.CommandPredicates

gsdCommandHandler :: CommandHandler
gsdCommandHandler logger eventStoreConnection persistedCommand@PersistedCommand {offset = offset , command = command } snapshotMaybe = do
  liftIO $ logInfo logger $ "processing " ++ (show persistedCommand) ++ " with " ++ (show snapshotMaybe)
  case (command) of
      command | commandsAreAllAlreadyProcessed offset snapshotMaybe ->
         liftIO $ logInfo logger $ "commands from workspace " ++ (show $ getAggregateId persistedCommand) ++ " all processed"
      command | isFirstCommandProcessed command snapshotMaybe  ->
         runStream $ handleFirstCommand logger eventStoreConnection $ fromJust $ fromCommand (command::Command)
      command | isCreateWorkspaceCommand command ->
         runStream $ handleCreateWorkspaceIsNotFirstCommandProcessedScenario logger eventStoreConnection snapshotMaybe $ fromJust $ fromCommand (command::Command)



handleFirstCommand ::  (IsStream stream,MonadIO (stream IO)) =>Logger -> EventStore.Connection -> GsdCommand -> stream IO ()
handleFirstCommand logger eventStoreConnection createWorkspace @ CreateWorkspace {commandId = commandId, workspaceId = workspaceId}  = do
  now <- liftIO $ getCurrentTime
  eventId <- liftIO $ Uuid.nextRandom
  serially $
    S.map ( \result -> ()) $
      (EventStream.persist logger eventStoreConnection  $ toEvent $ WorkspaceCreated {  eventId = eventId ,
                                                                                          createdOn = now,
                                                                                          workspaceId = workspaceId}) <>
      (SnapshotStream.persist logger eventStoreConnection AggregateSnapshot {lastOffsetConsumed = 0 ,
                                                          commandsProcessed = Set.fromList [commandId] ,
                                                          state = AggregateState { aggregateId = workspaceId }}) <>
      (CommandResponseStream.persist logger eventStoreConnection CommandSuccessfullyProcessed {
                                                                     commandId = commandId ,
                                                                     workspaceId = workspaceId })

handleCreateWorkspaceIsNotFirstCommandProcessedScenario :: (IsStream stream,MonadIO (stream IO)) =>Logger -> EventStore.Connection -> Maybe AggregateSnapshot -> GsdCommand ->  stream IO ()
handleCreateWorkspaceIsNotFirstCommandProcessedScenario logger eventStoreConnection snapshotMaybe createWorkspace @ CreateWorkspace {commandId = commandId, workspaceId = workspaceId} = do
  now <- liftIO $ getCurrentTime
  eventId <- liftIO $ Uuid.nextRandom
  serially $
    S.map ( \result -> ())  $
    (case snapshotMaybe of
        Nothing -> (SnapshotStream.persist logger eventStoreConnection AggregateSnapshot {lastOffsetConsumed = 0 ,
                             commandsProcessed = Set.fromList [commandId] ,
                             state = AggregateState { aggregateId = workspaceId }}) <>
                   (CommandResponseStream.persist logger eventStoreConnection CommandFailed { commandId = commandId ,
                                                   workspaceId = workspaceId ,
                                                   reason = "first command muste be CreateWorkspace "  })

        Just snapshot -> (SnapshotStream.persist logger eventStoreConnection AggregateSnapshot {
                                   lastOffsetConsumed = (lastOffsetConsumed snapshot) + 1 ,
                                   commandsProcessed = Set.insert commandId (commandsProcessed snapshot),
                                   state = state snapshot}) <>
                         (CommandResponseStream.persist logger eventStoreConnection CommandFailed {
                                                   commandId = commandId  ,
                                                   workspaceId = workspaceId,
                                                   reason = "scenario not handle yet"  }))
