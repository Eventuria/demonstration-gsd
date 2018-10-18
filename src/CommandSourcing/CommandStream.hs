{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module CommandSourcing.CommandStream (
PersistedCommand (..),
readForward,
persist,
subscribeToNewCommand) where

import CommandSourcing.Logger
import CommandSourcing.EventStore
import CommandSourcing.Core
import CommandSourcing.Commands
import Control.Concurrent.Async (wait)
import qualified Database.EventStore as EventStore
import qualified Data.Text as Text
import Data.UUID
import Data.Time

import qualified Data.UUID.V4 as Uuid
import Data.Maybe

import Data.Aeson
import Streamly
import qualified Streamly.Prelude as S
import Control.Concurrent
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Function ((&))

import CommandSourcing.Streams

data PersistedCommand = PersistedCommand { offset :: Offset , command :: Command }

instance Show PersistedCommand where
  show persistedCommand = "PersistedCommand { offset = " ++ ( show $ offset persistedCommand) ++ " , command = " ++ (show $ getCommandName $ command persistedCommand) ++ ":"
    ++ (show $ getWorkspaceId $ command persistedCommand) ++ " }"

subscribeToNewCommand :: (IsStream stream, MonadIO (stream IO), Semigroup (stream IO PersistedCommand)) => Logger -> EventStore.Connection -> WorkspaceId -> stream IO PersistedCommand
subscribeToNewCommand logger eventStoreConnection workSpaceId = do

              subscription <- liftIO $ EventStore.subscribe eventStoreConnection (getWorkspaceCommandStreamName workSpaceId) True Nothing
              _    <- liftIO $ EventStore.waitConfirmation subscription
              liftIO $ logInfo logger $ "subscription started for commands from workspace " ++ (show workSpaceId)
              loopNextCommand subscription logger where
              loopNextCommand subscription logger = do
                event <- liftIO $ EventStore.nextEvent subscription
                let commandMaybe = (EventStore.resolvedEventDataAsJson event) :: Maybe Command
                liftIO $ logInfo logger $ "command notified : " ++ (show commandMaybe)
                case commandMaybe of
                  Just command -> S.yield PersistedCommand { offset = toInteger $ EventStore.recordedEventNumber $ EventStore.resolvedEventOriginal event ,
                                                             command = command } <> (loopNextCommand subscription logger)

persist :: (IsStream stream, MonadIO (stream IO)) => Logger -> EventStore.Connection -> Command -> stream IO (Either PersistenceFailure PersistResult)
persist logger eventStoreConnection command =  do

    eventIdInEventStoreDomain <- liftIO $ Uuid.nextRandom

    let eventType  = EventStore.UserDefined $ Text.pack $ getCommandName command
        eventId = Just eventIdInEventStoreDomain
        eventData = EventStore.withJson command
        eventInEventStoreDomain = EventStore.createEvent eventType eventId eventData
    writeResult <- liftIO $ EventStore.sendEvent
            eventStoreConnection
            (getWorkspaceCommandStreamName $ workspaceId command)
            EventStore.anyVersion
            eventInEventStoreDomain
            getCredentials >>= wait

    liftIO $ logInfo logger $ "Command " ++ (getCommandName command) ++ " : command id " ++ (toString $ getCommandId command) ++ " persisted"
    S.yield $ Right $ PersistResult $ toInteger $ EventStore.writeNextExpectedVersion writeResult

readForward :: (IsStream stream, MonadIO (stream IO), Semigroup (stream IO PersistedCommand)) => EventStore.Connection -> WorkspaceId -> Maybe Offset -> stream IO PersistedCommand
readForward eventStoreConnection workspaceId fromOffset =  do
               let batchSize = 100 :: Integer
                   resolveLinkTos = False

               asyncRead <- liftIO $ EventStore.readStreamEventsForward
                                eventStoreConnection
                                (getWorkspaceCommandStreamName workspaceId)
                                (fromInteger $ fromMaybe 0 fromOffset)
                                (fromInteger batchSize)
                                resolveLinkTos
                                getCredentials
               commandFetched <- liftIO $ wait asyncRead
               case commandFetched of
                    EventStore.ReadSuccess readResult -> do
                        let commands = getCommandRequestFromResponse readResult
                        if (length commands) /= 0 then do
                            (S.fromList commands) <> (readForward eventStoreConnection workspaceId $ ( (+ batchSize) <$> Just (fromMaybe 0 fromOffset)))
                        else S.fromList $ commands
                    e -> error $ "Read failure: " <> show e



getCommandRequestFromResponse :: EventStore.StreamSlice -> [PersistedCommand]
getCommandRequestFromResponse eventSlice = (\event -> PersistedCommand { offset = toInteger $ EventStore.recordedEventNumber $ EventStore.resolvedEventOriginal $ event,
                                                                         command = (fromMaybe NonDeserializedCommand $ EventStore.resolvedEventDataAsJson event)}) <$> EventStore.sliceEvents eventSlice

getWorkspaceCommandStreamName :: WorkspaceId -> EventStore.StreamName
getWorkspaceCommandStreamName workspaceId = EventStore.StreamName $ Text.pack $ "workspace_command-" ++ toString workspaceId

