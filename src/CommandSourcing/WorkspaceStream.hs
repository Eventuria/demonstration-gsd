{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module CommandSourcing.WorkspaceStream (
streamAllInfinitely,
streamAll,
streamFromOffset,
subscribeToNewWorkspaceCreated
) where

import Prelude hiding (catch)
import CommandSourcing.EventStore
import CommandSourcing.Commands
import CommandSourcing.CommandResponse
import CommandSourcing.Events
import Data.Time
import Data.Maybe
import qualified Data.UUID.V4 as Uuid
import qualified CommandSourcing.EventStream as EventStream
import qualified CommandSourcing.CommandStream as CommandStream
import qualified CommandSourcing.CommandResponseStream as CommandResponseStream
import Control.Concurrent.Async (wait)
import Conduit
import Data.Set (Set)
import qualified Data.Set as Set
import CommandSourcing.Core
import qualified Database.EventStore as EventStore
import System.Log.Logger
import qualified Data.Text as Text
import Data.UUID
import Data.ByteString.Char8 as Char8 (unpack)
import qualified Control.Concurrent as Concurrent
import Control.Exception
import Control.Concurrent hiding (yield)
import Data.Conduit

type CorruptedStreamName = String

getWorkspaceCreatedStreamName :: EventStore.StreamName
getWorkspaceCreatedStreamName = EventStore.StreamName $ Text.pack $ "$et-createWorkspace"

subscribeToNewWorkspaceCreatedWithCorruptedStreams :: EventStore.Connection ->  ConduitT () (Offset , Either CorruptedStreamName WorkspaceId) IO()
subscribeToNewWorkspaceCreatedWithCorruptedStreams eventStoreConnection =  do
              let logger = "[gsd.persist.command.request]"
              liftIO $ updateGlobalLogger logger $ setLevel INFO
              liftIO $ infoM logger "[workspaceStream.subscription] - subscribing to new workspace created"
              subscription <- liftIO $ EventStore.subscribe eventStoreConnection getWorkspaceCreatedStreamName True Nothing
              result <- liftIO $ (try $ EventStore.waitConfirmation subscription )
              case result of
                Left e @ SomeException {} -> do
                           liftIO $ infoM logger "[workspaceStream.subscription] - subscription to new workspace created stream failed!"
                           liftIO $ threadDelay (5 * 1000000) -- 5 seconds
                           subscribeToNewWorkspaceCreatedWithCorruptedStreams eventStoreConnection
                Right _ -> do
                           liftIO $ infoM logger "[workspaceStream.subscription] - subscription started for commands handlers"
                           loopNextEvent subscription where
                           loopNextEvent subscription = do
                              resolvedEvent <- liftIO $ EventStore.nextEvent subscription
                              liftIO $ infoM logger $ "[workspaceStream.subscription] - new workspace created stream event triggered : " ++ (show resolvedEvent)
                              yield $ getCreatedWorkspace $ (EventStore.resolvedEventOriginal resolvedEvent)
                              loopNextEvent subscription

waitSubscriptionConfirmation :: EventStore.Subscription s => s -> IO (Either SomeException ())
waitSubscriptionConfirmation subscription  = do
               result <- try $ EventStore.waitConfirmation subscription
               return $ case result of
                 Left e -> Left e
                 Right _ -> Right ()


discardCorruptedStreams :: ConduitT (Offset , Either CorruptedStreamName WorkspaceId) WorkspaceId IO ()
discardCorruptedStreams = awaitForever $ \(offset, workSpaceIdEither) -> do
              let logger = "[gsd.persist.command.request]"
              liftIO $ updateGlobalLogger logger $ setLevel INFO
              case workSpaceIdEither of
                Right workSpaceId -> yield workSpaceId
                Left corruptedStreamName -> liftIO $ infoM logger $ "[workspaceStream.discardCorruptedStreams] detected corrupted stream name " ++ (show offset) ++ " : " ++ corruptedStreamName


streamAll :: EventStore.Connection -> ConduitT () WorkspaceId IO ()
streamAll  eventStoreConnection = do
    yield eventStoreConnection
    .| streamAllWithCorruptedStreams
    .| discardCorruptedStreams

streamAllInfinitely :: EventStore.Connection -> ConduitT () WorkspaceId IO ()
streamAllInfinitely  eventStoreConnection = do
    yield eventStoreConnection
      .| streamAllWithCorruptedStreams
      .| discardCorruptedStreams
    subscribeToNewWorkspaceCreated eventStoreConnection

subscribeToNewWorkspaceCreated :: EventStore.Connection -> ConduitT () WorkspaceId IO ()
subscribeToNewWorkspaceCreated eventStoreConnection = do
  (subscribeToNewWorkspaceCreatedWithCorruptedStreams eventStoreConnection)
   .| discardCorruptedStreams


streamAllWithCorruptedStreams :: ConduitT EventStore.Connection (Offset,Either CorruptedStreamName WorkspaceId) IO()
streamAllWithCorruptedStreams = awaitForever $ \eventStoreConnection -> do
                              yield (eventStoreConnection,0)
                              .| streamFromOffset

streamFromOffset :: ConduitT (EventStore.Connection,Offset) (Offset,Either CorruptedStreamName WorkspaceId) IO()
streamFromOffset = awaitForever $ \(eventStoreConnection,fromOffset) -> do
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
                            yieldMany commandStreamNamesByWorkspaceId
                            leftover (eventStoreConnection , fromOffset + batchSize)
                            streamFromOffset
                        else yieldMany $ commandStreamNamesByWorkspaceId
                    EventStore.ReadNoStream -> return ()
                    e -> error $ "Read failure: " <> show e

getCreatedWorkspace :: EventStore.RecordedEvent -> (Offset,Either CorruptedStreamName WorkspaceId)
getCreatedWorkspace recordedEvent = (toInteger $ EventStore.recordedEventNumber recordedEvent,
                                      case (fromString $ drop (length  ("0@workspace_command-" :: [Char])) $ unpack $ EventStore.recordedEventData recordedEvent) of
                                          Just workSpaceId -> Right workSpaceId
                                          Nothing -> Left $ unpack $ EventStore.recordedEventData recordedEvent )

