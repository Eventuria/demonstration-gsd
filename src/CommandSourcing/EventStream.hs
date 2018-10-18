{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module CommandSourcing.EventStream (
readForward,
persist) where

import Streamly
import qualified Streamly.Prelude as S
import Control.Concurrent
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function ((&))
import CommandSourcing.EventStore
import qualified Database.EventStore as EventStore
import CommandSourcing.Core
import CommandSourcing.Events
import CommandSourcing.Streams
import CommandSourcing.Logger
import Control.Concurrent.Async (wait)

import qualified Data.Text as Text
import Data.UUID
import Data.Time
import qualified Data.UUID.V4 as Uuid
import Data.Maybe

import Data.Aeson



persist :: (IsStream stream, MonadIO (stream IO)) => Logger -> EventStore.Connection -> WorkspaceEvent -> stream IO (Either PersistenceFailure PersistResult)
persist logger eventStoreConnection workspaceEvent =  do

    eventIdInEventStoreDomain <- liftIO $ Uuid.nextRandom
    let eventType  = EventStore.UserDefined $ Text.pack $ getEventName workspaceEvent
        eventData = EventStore.withJson workspaceEvent
        eventInEventStoreDomain = EventStore.createEvent eventType (Just eventIdInEventStoreDomain) eventData
    writeResult <- liftIO $ EventStore.sendEvent
            eventStoreConnection
            (getWorkspaceEventStreamName $ workspaceId workspaceEvent)
            EventStore.anyVersion
            eventInEventStoreDomain
            getCredentials >>= wait

    liftIO $ logInfo logger $ "Event " ++ (getEventName workspaceEvent) ++ " : id " ++ (toString $ getEventId workspaceEvent) ++ " persisted"
    S.yield $ Right $ PersistResult $ toInteger $ EventStore.writeNextExpectedVersion writeResult

readForward :: (IsStream stream, MonadIO (stream IO), Semigroup (stream IO WorkspaceEvent)) => EventStore.Connection -> WorkspaceId -> Offset -> stream IO WorkspaceEvent
readForward eventStoreConnection  workSpaceId fromOffset = do
               let batchSize = 100 :: Integer
                   resolveLinkTos = False
               asyncRead <- liftIO $ EventStore.readStreamEventsForward
                                eventStoreConnection
                                (getWorkspaceEventStreamName workSpaceId)
                                (fromInteger fromOffset)
                                (fromInteger batchSize)
                                resolveLinkTos
                                getCredentials
               eventFetched <- liftIO $ wait asyncRead
               case eventFetched of
                    EventStore.ReadSuccess readResult -> do
                        let events = getEventsFromResponse readResult
                        if (length events) /= 0 then do
                            (S.fromList events) <> (readForward eventStoreConnection workSpaceId $ fromOffset + batchSize)
                        else S.fromList events
                    e -> error $ "Read failure: " <> show e


getEventsFromResponse :: EventStore.StreamSlice -> [WorkspaceEvent]
getEventsFromResponse sl = catMaybes $ EventStore.resolvedEventDataAsJson <$> EventStore.sliceEvents sl

getWorkspaceEventStreamName :: WorkspaceId -> EventStore.StreamName
getWorkspaceEventStreamName workspaceId = EventStore.StreamName $ Text.pack $ "workspace_event-" ++ toString workspaceId

