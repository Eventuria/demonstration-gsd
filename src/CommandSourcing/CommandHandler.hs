{-# LANGUAGE DuplicateRecordFields #-}
--{-# LANGUAGE DataKinds                 #-}
--{-# LANGUAGE DeriveDataTypeable        #-}
--{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
--{-# LANGUAGE RecordWildCards           #-}
--{-# LANGUAGE ScopedTypeVariables, RankNTypes, FlexibleContexts, BangPatterns #-}

module CommandSourcing.CommandHandler  where

import CommandSourcing.Commands
import qualified CommandSourcing.Commands as Commands
import CommandSourcing.CommandResponse
import CommandSourcing.Events
import CommandSourcing.Snapshot
import Data.Time
import Data.Maybe
import qualified Data.UUID.V4 as Uuid
import qualified CommandSourcing.EventStream as EventStream
import qualified CommandSourcing.CommandStream as CommandStream
import qualified CommandSourcing.CommandResponseStream as CommandResponseStream
import qualified CommandSourcing.WorkspaceStream as WorkspaceStream
import qualified CommandSourcing.SnapshotStream as SnapshotStream
import Control.Concurrent.Async (wait)
import CommandSourcing.Core


import qualified Database.EventStore as EventStore
import System.Log.Logger
import qualified Data.Text as Text
import Data.UUID
import Data.ByteString.Char8 as Char8 (unpack)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Conduit.TMChan
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.List hiding (sinkNull)
import Conduit
import Foundation.Conduit (bracketConduit)
import Control.Concurrent.STM
import Control.Concurrent (forkIO, threadDelay)
type CorruptedStreamName = String




startHandlers :: EventStore.Connection -> ResourceT IO ()
startHandlers connection = do
        let logger = "[gsd.main]"
        liftIO $ updateGlobalLogger logger $ setLevel INFO

        (_, workspaceProcessingRequiredChannel) <- allocate (newTMChanIO)
                                             (atomically . closeTMChan)

        lift $ forkIO .runConduit $ (WorkspaceStream.streamAllInfinitely connection)
                                    .| sinkTMChan workspaceProcessingRequiredChannel

--        lift $ forkIO .runConduit $ (WorkspaceStream.streamAllInfinitely connection)
--                                    .| (CommandStream.subscribeToNewCommand connection)
--                                    .| mapC (\command -> getWorkspaceId command)
--                                    .| sinkTMChan workspaceProcessingRequiredChannel

--        lift $ forkIO .runConduit $ sourceTMChan workspaceCreatedChannel
--                                    .| iterMC (\workSpaceId -> do
--                                           liftIO $ infoM logger $ "[processCommand] - subscribing to workspace : " ++ (show  workSpaceId))
--                                    .| (CommandStream.subscribeToNewCommand connection)
--                                    .| mapC (\command -> getWorkspaceId command)
--                                    .| iterMC (\workSpaceId -> do
--                                           liftIO $ infoM logger $ "[processCommand] - new command detected from workspace : " ++ (show  workSpaceId))
--                                    .| sinkTMChan workspaceProcessingRequiredChannel

        lift $ runConduit $ sourceTMChan workspaceProcessingRequiredChannel
                                    .| (\workSpaceId -> forkIO $ yield $ workSpaceId .| processCommand connection  )
                                    .| sinkNull
        return ()



processCommand :: EventStore.Connection -> ConduitT WorkspaceId () IO ()
processCommand eventStoreConnection = awaitForever $ \workSpaceId -> do
              let logger = "[gsd.main]"
              liftIO $ updateGlobalLogger logger $ setLevel INFO
              liftIO $ infoM logger $ "[processCommand] - processing commands for workspace : " ++ (show  workSpaceId)
              liftIO $ threadDelay (5 * 1000000) -- 5 seconds
              yield (eventStoreConnection, workSpaceId)
                .| SnapshotStream.retrieveLastSnapshot
                .| (awaitForever $ \snapshotMaybe -> do
                       yield (eventStoreConnection, workSpaceId , fromMaybe 0 (lastOffsetConsumed <$> snapshotMaybe) )
                       .| CommandStream.readForward
                       .| mapC (\command -> (eventStoreConnection,command,snapshotMaybe)))
                       .| handleCommand

handleCommand :: ConduitT (EventStore.Connection, Command, Maybe Snapshot) () IO ()
handleCommand = awaitForever $ \(eventStoreConnection,commandRequest, snapshotMaybe) -> do
            now <- liftIO $ getCurrentTime
            eventId <- liftIO $ Uuid.nextRandom
            case commandRequest of
                commandToProcess@CreateWorkspace { commandId = commandId, workspaceId = workspaceId } | Nothing == snapshotMaybe ->
                          yield ( eventStoreConnection , WorkspaceCreated { createdOn = now,
                                                                            eventId = eventId ,
                                                                            commandId = commandId,
                                                                            workspaceId = workspaceId})
                           .| EventStream.persist
                           .| mapC (\response -> (eventStoreConnection, Snapshot {lastOffsetConsumed = 0 ,
                                                                                  commandsProcessed = Set.fromList [commandId] ,
                                                                                  state = WorkspaceState { workspaceId = workspaceId }}))
                           .| SnapshotStream.persist
                           .| mapC (\response -> (eventStoreConnection, CommandSuccessfullyProcessed {
                                                                                commandId = getCommandId commandRequest ,
                                                                                workspaceId = getWorkspaceId commandRequest }))
                           .| CommandResponseStream.persist
                           .| mapC (\response -> ())
                commandRequest ->
                          yield (case snapshotMaybe of
                                        Nothing -> (eventStoreConnection, Snapshot {lastOffsetConsumed = 0 ,
                                                             commandsProcessed = Set.fromList [Commands.commandId commandRequest] ,
                                                             state = WorkspaceState { workspaceId = Commands.workspaceId commandRequest }})
                                        Just snapshot -> (eventStoreConnection, Snapshot {lastOffsetConsumed = (lastOffsetConsumed snapshot) + 1 ,
                                                                   commandsProcessed = Set.insert (Commands.commandId commandRequest) (commandsProcessed snapshot),
                                                                   state = state snapshot}))
                            .| SnapshotStream.persist
                            .| mapC (\response -> (eventStoreConnection, CommandFailed { commandId = getCommandId commandRequest ,
                                                                       workspaceId = getWorkspaceId commandRequest,
                                                                       reason = "scenario not handle yet"  }))
                            .| CommandResponseStream.persist
                            .| mapC (\response -> ())














