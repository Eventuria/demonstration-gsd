{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
module CommandSourcing.CommandHandler  where

import CommandSourcing.Commands
import qualified CommandSourcing.Commands as Commands
import qualified CommandSourcing.CommandResponse as CommandResponse
import CommandSourcing.Events
import CommandSourcing.Snapshot
import Data.Time
import Data.Maybe
import qualified Data.UUID.V4 as Uuid
import qualified CommandSourcing.EventStream as EventStream
import qualified CommandSourcing.WorkspaceStream as WorkspaceStream
import qualified CommandSourcing.CommandStream as CommandStream
import qualified CommandSourcing.CommandResponseStream as CommandResponseStream
import qualified CommandSourcing.SnapshotStream as SnapshotStream
import Control.Concurrent.Async (wait)
import CommandSourcing.Core
import qualified Data.Text as Text
import Data.UUID
import Data.ByteString.Char8 as Char8 (unpack)
import Data.Set (Set)
import qualified Data.Set as Set

import Streamly
import qualified Streamly.Prelude as S
import Control.Concurrent
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function ((&))

import System.Random
import Data.UUID
import qualified Database.EventStore as EventStore
import Control.Exception
import CommandSourcing.Logger
import CommandSourcing.Commands
import Data.Semigroup (Semigroup(..))
import CommandSourcing.Streams

type CorruptedStreamName = String



startHandlers :: Logger -> EventStore.Connection -> IO ()
startHandlers logger eventStoreConnection = do
  logInfo logger "starting streams"
  runStream $ parallely $ (WorkspaceStream.streamAllInfinitely logger eventStoreConnection) & S.mapM (\persistedWorkspace -> do
    liftIO $ logInfo logger $ "detected workspace id : " ++ (show persistedWorkspace)
    runStream $ serially $ WorkspaceStream.yieldAndSubscribeToWorkspaceUpdates logger eventStoreConnection persistedWorkspace
      & S.mapM (\WorkspaceStream.PersistedWorkspace {workspaceIdPersisted = workspaceId}  -> do
        runStream $ processCommand logger eventStoreConnection workspaceId ))


processCommand :: (IsStream stream, MonadIO (stream IO )) =>  Logger -> EventStore.Connection -> WorkspaceId -> stream IO ()
processCommand logger eventStoreConnection workspaceId = do
  liftIO $ logInfo logger $ "processing commands workspace for " ++ (show workspaceId)
  (SnapshotStream.retrieveLastOffsetConsumed eventStoreConnection workspaceId)
    & S.mapM ( \lastOffsetConsumed -> do
      runStream $ (CommandStream.readForward eventStoreConnection workspaceId lastOffsetConsumed)
        & S.mapM (\persistedCommand ->
           runStream $ (SnapshotStream.retrieveLast eventStoreConnection workspaceId)
              & S.mapM (\snapshotMaybe ->
                runStream $ handleCommand logger eventStoreConnection persistedCommand snapshotMaybe)))

handleCommand :: (IsStream stream, MonadIO (stream IO)) =>  Logger -> EventStore.Connection -> CommandStream.PersistedCommand -> Maybe Snapshot -> stream IO ()
handleCommand logger eventStoreConnection persistedCommand snapshotMaybe = do
  now <- liftIO $ getCurrentTime
  liftIO $ logInfo logger $ "processing " ++ (show persistedCommand) ++ " with " ++ (show snapshotMaybe)
  case (persistedCommand) of
      CommandStream.PersistedCommand { offset = offset , command = command} | Just offset == (lastOffsetConsumed <$> snapshotMaybe) -> do
          liftIO $ logInfo logger $ "commands from workspace " ++ (show $ getWorkspaceId command) ++ " all processed"
          return ()
      CommandStream.PersistedCommand { offset = offset , command = commandToProcess@CreateWorkspace { commandId = commandId, workspaceId = workspaceId }} | Nothing == snapshotMaybe -> do
          eventId <- liftIO $ Uuid.nextRandom
          serially $ S.map ( \result -> ()) $ (EventStream.persist logger eventStoreConnection  WorkspaceCreated { createdOn = now,
                                                            eventId = eventId ,
                                                            commandId = commandId,
                                                            workspaceId = workspaceId}) <>
                     (SnapshotStream.persist logger eventStoreConnection Snapshot {lastOffsetConsumed = 0 ,
                                                                  commandsProcessed = Set.fromList [commandId] ,
                                                                  state = WorkspaceState { workspaceId = workspaceId }}) <>
                     (CommandResponseStream.persist logger eventStoreConnection CommandResponse.CommandSuccessfullyProcessed {
                                                                                                     commandId = getCommandId commandToProcess ,
                                                                                                     workspaceId = getWorkspaceId commandToProcess })

      CommandStream.PersistedCommand { command = commandToProcess@CreateWorkspace { }} -> do
          serially $ S.map ( \result -> ())  $ (case snapshotMaybe of
                        Nothing -> (SnapshotStream.persist logger eventStoreConnection Snapshot {lastOffsetConsumed = 0 ,
                                             commandsProcessed = Set.fromList [Commands.commandId commandToProcess] ,
                                             state = WorkspaceState { workspaceId = Commands.workspaceId commandToProcess }}) <>
                                   (CommandResponseStream.persist logger eventStoreConnection CommandResponse.CommandFailed { commandId = getCommandId commandToProcess ,
                                                                   workspaceId = getWorkspaceId commandToProcess,
                                                                   reason = "first command muste be CreateWorkspace "  })

                        Just snapshot -> (SnapshotStream.persist logger eventStoreConnection Snapshot {lastOffsetConsumed = (lastOffsetConsumed snapshot) + 1 ,
                                                   commandsProcessed = Set.insert (Commands.commandId commandToProcess) (commandsProcessed snapshot),
                                                   state = state snapshot}) <>
                                         (CommandResponseStream.persist logger eventStoreConnection CommandResponse.CommandFailed { commandId = getCommandId commandToProcess ,
                                                                   workspaceId = getWorkspaceId commandToProcess,
                                                                   reason = "scenario not handle yet"  }))











