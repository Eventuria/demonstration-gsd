{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module CommandSourcing.WorkspaceStream (
PersistedWorkspace(..),
streamAllInfinitely,
streamAll,
streamFromOffset,
subscribeToNewWorkspaceCreated,
subscribeToWorkspaceUpdates,
yieldAndSubscribeToWorkspaceUpdates

) where

import qualified CommandSourcing.CommandStream as CommandStream
import Prelude hiding (catch)
import CommandSourcing.EventStore
import CommandSourcing.Commands
import CommandSourcing.Events
import Data.Time
import Data.Maybe
import qualified Data.UUID.V4 as Uuid
import Control.Concurrent.Async (wait)

import Data.Set (Set)
import qualified Data.Set as Set
import CommandSourcing.Core
import qualified Database.EventStore as EventStore
import CommandSourcing.Logger
import qualified Data.Text as Text
import Data.UUID
import Data.ByteString.Char8 as Char8 (unpack)
import qualified Control.Concurrent as Concurrent
import Control.Exception

import Streamly
import qualified Streamly.Prelude as S
import Control.Concurrent
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Function ((&))
import Data.Either
type CorruptedStreamName = String

data PersistedWorkspace = PersistedWorkspace { offset :: Offset, workspaceIdPersisted :: WorkspaceId } deriving (Show,Eq)

getWorkspaceCreatedStreamName :: EventStore.StreamName
getWorkspaceCreatedStreamName = EventStore.StreamName $ Text.pack $ "$et-createWorkspace"


yieldAndSubscribeToWorkspaceUpdates :: (IsStream stream, MonadIO (stream IO), Semigroup (stream IO PersistedWorkspace), Semigroup (stream IO CommandStream.PersistedCommand ))  =>
                                       Logger -> EventStore.Connection -> PersistedWorkspace -> stream IO PersistedWorkspace
yieldAndSubscribeToWorkspaceUpdates logger eventStoreConnection workspacePersisted =
  (S.yield workspacePersisted) <>
  ((subscribeToWorkspaceUpdates logger eventStoreConnection $ workspaceIdPersisted workspacePersisted )
    & S.map (\workspaceId -> PersistedWorkspace { offset = offset workspacePersisted ,
                                                     workspaceIdPersisted = workspaceId }))


subscribeToWorkspaceUpdates :: (IsStream stream, MonadIO (stream IO), Semigroup (stream IO CommandStream.PersistedCommand ))  =>
                               Logger -> EventStore.Connection -> WorkspaceId -> stream IO WorkspaceId
subscribeToWorkspaceUpdates logger  eventStoreConnection workspaceId =
  ((CommandStream.subscribeToNewCommand logger eventStoreConnection workspaceId) & S.map (\command -> workspaceId))

subscribeToNewWorkspaceCreated :: (IsStream stream, MonadIO (stream IO), Semigroup (stream IO PersistedWorkspace))  =>
                                  Logger -> EventStore.Connection ->  stream IO PersistedWorkspace
subscribeToNewWorkspaceCreated logger eventStoreConnection = do
              liftIO $ logInfo logger "subscribing to new workspace created notifier"
              subscription <- liftIO $ EventStore.subscribe eventStoreConnection getWorkspaceCreatedStreamName True Nothing
              result <- liftIO $ (try $ EventStore.waitConfirmation subscription )
              case result of
                Left e @ SomeException {} -> do
                           liftIO $ logInfo logger "subscription to new workspace created stream failed!"
                           liftIO $ threadDelay (5 * 1000000) -- 5 seconds
                           subscribeToNewWorkspaceCreated logger eventStoreConnection
                Right _ -> do
                           liftIO $ logInfo logger "subscription started for commands handlers"
                           loopNextEvent subscription where
                           loopNextEvent subscription = do
                              resolvedEvent <- liftIO $ EventStore.nextEvent subscription
                              liftIO $ logInfo logger $ "new workspace created stream event triggered : " ++ (show resolvedEvent)
                              (S.yield $ getCreatedWorkspace $ (EventStore.resolvedEventOriginal resolvedEvent)) <> loopNextEvent subscription

waitSubscriptionConfirmation :: EventStore.Subscription s => s -> IO (Either SomeException ())
waitSubscriptionConfirmation subscription  = do
               result <- try $ EventStore.waitConfirmation subscription
               return $ case result of
                 Left e -> Left e
                 Right _ -> Right ()

streamAll :: (IsStream stream, MonadIO (stream IO),Semigroup (stream IO PersistedWorkspace)) => Logger -> EventStore.Connection -> stream IO PersistedWorkspace
streamAll logger eventStoreConnection = streamFromOffset logger eventStoreConnection 0

streamAllInfinitely ::  (IsStream stream, MonadIO (stream IO),Semigroup (stream IO PersistedWorkspace)) => Logger -> EventStore.Connection -> stream IO PersistedWorkspace
streamAllInfinitely  logger eventStoreConnection = (subscribeToNewWorkspaceCreated logger eventStoreConnection) `parallel` (streamAll logger eventStoreConnection)


streamFromOffset :: (IsStream stream, MonadIO (stream IO), Semigroup (stream IO PersistedWorkspace)) => Logger -> EventStore.Connection -> Offset ->  stream IO PersistedWorkspace
streamFromOffset logger eventStoreConnection fromOffset = do
               liftIO $ logInfo logger $ "starting streaming commands from offset " ++ (show fromOffset)
               let batchSize = 100 :: Integer
                   resolveLinkTos = False
               asyncRead <- liftIO $ EventStore.readStreamEventsForward
                                eventStoreConnection
                                getWorkspaceCreatedStreamName
                                (fromInteger fromOffset)
                                (fromInteger batchSize)
                                resolveLinkTos
                                getCredentials
               commandFetched <- liftIO $ wait asyncRead
               case commandFetched of
                    EventStore.ReadSuccess readResult -> do
                        let commandStreamNamesByWorkspaceId = getCreatedWorkspace
                                                              <$> EventStore.resolvedEventOriginal
                                                              <$> EventStore.sliceEvents readResult
                        if (length commandStreamNamesByWorkspaceId) /= 0 then do
                            (S.fromList commandStreamNamesByWorkspaceId) <> (streamFromOffset logger eventStoreConnection $ fromOffset + batchSize)
                        else S.fromList commandStreamNamesByWorkspaceId
                    EventStore.ReadNoStream -> error $ "ReadNoStream "
                    e -> error $ "Read failure: " <> show e

getCreatedWorkspace :: EventStore.RecordedEvent -> PersistedWorkspace
getCreatedWorkspace recordedEvent =
  PersistedWorkspace { offset = toInteger $ EventStore.recordedEventNumber recordedEvent,
                       workspaceIdPersisted = case (fromString $ drop (length  ("0@workspace_command-" :: [Char])) $ unpack $ EventStore.recordedEventData recordedEvent) of
                                          Just workspaceId -> workspaceId
                                          Nothing -> error $ "Workspace stream corrupted :" ++ (unpack $ EventStore.recordedEventData recordedEvent) }

