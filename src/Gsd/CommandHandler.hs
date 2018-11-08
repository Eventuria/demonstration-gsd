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
gsdCommandHandler logger eventStoreConnection persistedCommand@PersistedCommand {offset = offset , command = command } snapshotMaybe
   | commandsAreAllAlreadyProcessed offset snapshotMaybe =
         liftIO $ logInfo logger $ "commands from workspace " ++ (show $ getAggregateId persistedCommand) ++ " all processed"
   | isFirstCommandProcessed command snapshotMaybe  =
         handleFirstCommand logger eventStoreConnection $ fromJust $ fromCommand (command::Command)
   | (isCreateWorkspaceCommand command) && (snapshotMaybe /= Nothing) =
         handleCreateWorkspaceIsNotFirstCommandProcessedScenario logger eventStoreConnection snapshotMaybe $ fromJust $ fromCommand (command::Command)
   | otherwise = unknownCommandFlow logger eventStoreConnection snapshotMaybe $ command



handleFirstCommand ::  Logger -> EventStore.Connection -> GsdCommand -> IO ()
handleFirstCommand logger eventStoreConnection createWorkspace @ CreateWorkspace {commandId = commandId, workspaceId = workspaceId}  = do
  now <- liftIO $ getCurrentTime
  eventId <- liftIO $ Uuid.nextRandom
  EventStream.persist logger eventStoreConnection  $ toEvent $ WorkspaceCreated {  eventId = eventId ,
                                                                                      createdOn = now,
                                                                                      workspaceId = workspaceId}
  SnapshotStream.persist logger eventStoreConnection AggregateSnapshot {lastOffsetConsumed = 0 ,
                                                      commandsProcessed = Set.fromList [commandId] ,
                                                      state = AggregateState { aggregateId = workspaceId }}
  CommandResponseStream.persist logger eventStoreConnection CommandSuccessfullyProcessed {
                                                                 commandId = commandId ,
                                                                 workspaceId = workspaceId }
  return ()

handleCreateWorkspaceIsNotFirstCommandProcessedScenario :: Logger -> EventStore.Connection -> Maybe AggregateSnapshot -> GsdCommand ->  IO ()
handleCreateWorkspaceIsNotFirstCommandProcessedScenario logger eventStoreConnection Nothing createWorkspace @ CreateWorkspace {commandId = commandId, workspaceId = workspaceId} = do
     now <- liftIO $ getCurrentTime
     eventId <- liftIO $ Uuid.nextRandom
     SnapshotStream.persist logger eventStoreConnection AggregateSnapshot {lastOffsetConsumed = 0 ,
               commandsProcessed = Set.fromList [commandId] ,
               state = AggregateState { aggregateId = workspaceId }}
     CommandResponseStream.persist logger eventStoreConnection CommandFailed { commandId = commandId ,
                                     workspaceId = workspaceId ,
                                     reason = "first command muste be CreateWorkspace "  }
     return ()

unknownCommandFlow :: Logger -> EventStore.Connection -> Maybe AggregateSnapshot -> Command ->  IO ()
unknownCommandFlow logger eventStoreConnection (Just snapshot) command @ Command { commandHeader = CommandHeader { commandId = commandId , aggregateId = workspaceId}}  = do
      SnapshotStream.persist logger eventStoreConnection AggregateSnapshot {
               lastOffsetConsumed = (lastOffsetConsumed snapshot) + 1 ,
               commandsProcessed = Set.insert commandId (commandsProcessed snapshot),
               state = state snapshot}
      CommandResponseStream.persist logger eventStoreConnection CommandFailed {
                               commandId = commandId  ,
                               workspaceId = workspaceId,
                               reason = "scenario not handle yet"  }
      return ()
