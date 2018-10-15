{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
module CommandSourcing.SnapshotStream (
retrieveLastSnapshot,
persist
) where

import CommandSourcing.Core
import Control.Concurrent.Async (wait)
import qualified Database.EventStore as EventStore
import qualified Data.Text as Text
import Data.UUID
import Data.Time
import System.Log.Logger
import qualified Data.UUID.V4 as Uuid
import Data.Maybe
import Conduit
import qualified Data.Conduit.Combinators as ConduitCombinators
import qualified Data.Conduit.List as ConduitList
import System.Log.Logger
import Data.Aeson
import CommandSourcing.Snapshot

data PersistenceFailure = CommandAlreadyPersisted
data PersistResult = PersistResult {writeNextVersion :: Integer}

retrieveLastSnapshot :: ConduitT (EventStore.Connection,WorkspaceId) (Maybe Snapshot)  IO()
retrieveLastSnapshot = awaitForever $ \(eventStoreConnection , workspaceId) ->  do
        let credentials = Nothing
            resolveLinkTos = False
        readResult <- liftIO $ EventStore.readStreamEventsBackward
                    eventStoreConnection
                    (getStreamName workspaceId)
                    (fromInteger 0)
                    (fromInteger 1)
                    resolveLinkTos
                    credentials >>= wait
        case readResult of
          EventStore.ReadSuccess responseContent -> do
              let snapshots = getSnapshotsFromResponse responseContent
              yield $ listToMaybe snapshots
          EventStore.ReadNoStream -> yield Nothing
          e -> error $ "Read failure: " <> show e

getSnapshotsFromResponse :: EventStore.StreamSlice -> [Snapshot]
getSnapshotsFromResponse sl = catMaybes $ EventStore.resolvedEventDataAsJson <$> EventStore.sliceEvents sl

persist :: ConduitT (EventStore.Connection, Snapshot) (Either PersistenceFailure PersistResult) IO()
persist = awaitForever $ \(eventStoreConnection , snapshot) ->  do

    let logger = "[gsd.persist.command.request]"
    liftIO $ updateGlobalLogger logger $ setLevel INFO

    eventIdInEventStoreDomain <- liftIO $ Uuid.nextRandom

    let credentials = Nothing
        eventType  = EventStore.UserDefined $ Text.pack $  "workspaceState"
        eventId = Just eventIdInEventStoreDomain
        eventData = EventStore.withJson snapshot
        eventInEventStoreDomain = EventStore.createEvent eventType eventId eventData
    writeResult <- liftIO $ EventStore.sendEvent
            eventStoreConnection
            (getStreamName $ workspaceId $ state snapshot)
            EventStore.anyVersion
            eventInEventStoreDomain
            credentials >>= wait

    liftIO $ infoM logger "snapshot persisted"
    yield $ Right $ PersistResult $ toInteger $ EventStore.writeNextExpectedVersion writeResult


getStreamName :: WorkspaceId -> EventStore.StreamName
getStreamName workspaceId = EventStore.StreamName $ Text.pack $ "workspace_snapshot-" ++ toString workspaceId



instance ToJSON WorkspaceState where
   toJSON (WorkspaceState workspaceId) = object ["workspaceId" .= workspaceId]

instance ToJSON Snapshot where
   toJSON (Snapshot lastOffsetConsumed commandsProcessed state) = object [
      "lastOffsetConsumed" .= lastOffsetConsumed,
      "commandsProcessed" .= commandsProcessed,
      "state" .= state
      ]

instance FromJSON Snapshot  where

    parseJSON (Object jsonObject) = Snapshot <$> jsonObject .: "lastOffsetConsumed"
                                             <*> jsonObject .: "commandsProcessed"
                                             <*> jsonObject .: "state"

instance FromJSON WorkspaceState  where

    parseJSON (Object jsonObject) = WorkspaceState <$> jsonObject .: "workspaceId"