{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
module CommandSourcing.SnapshotStream (
retrieveLastOffsetConsumed,
retrieveLast,
persist
) where

import Streamly
import qualified Streamly.Prelude as S
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function ((&))
import Control.Concurrent

import CommandSourcing.Logger
import CommandSourcing.EventStore
import CommandSourcing.Core
import Control.Concurrent.Async (wait)
import qualified Database.EventStore as EventStore
import qualified Data.Text as Text
import Data.UUID
import Data.Time

import qualified Data.UUID.V4 as Uuid
import Data.Maybe

import Data.Aeson
import CommandSourcing.Snapshot
import CommandSourcing.Streams


retrieveLastOffsetConsumed :: (IsStream stream, MonadIO (stream IO)) => EventStore.Connection -> WorkspaceId -> stream IO (Maybe Offset)
retrieveLastOffsetConsumed eventStoreConnection workspaceId =
  (retrieveLast eventStoreConnection workspaceId) & S.map (\snapshotMaybe -> lastOffsetConsumed <$> snapshotMaybe)

retrieveLast :: (IsStream stream, MonadIO (stream IO)) => EventStore.Connection -> WorkspaceId -> stream IO (Maybe Snapshot)
retrieveLast eventStoreConnection workspaceId =  do
        let resolveLinkTos = False
        readResult <- liftIO $ EventStore.readStreamEventsBackward
                    eventStoreConnection
                    (getStreamName workspaceId)
                    (fromInteger 0)
                    (fromInteger 1)
                    resolveLinkTos
                    getCredentials >>= wait
        case readResult of
          EventStore.ReadSuccess responseContent -> do
              let snapshots = getSnapshotsFromResponse responseContent
              S.yield $ listToMaybe snapshots
          EventStore.ReadNoStream -> S.yield Nothing
          e -> error $ "Read failure: " <> show e

getSnapshotsFromResponse :: EventStore.StreamSlice -> [Snapshot]
getSnapshotsFromResponse sl = catMaybes $ EventStore.resolvedEventDataAsJson <$> EventStore.sliceEvents sl

persist :: (IsStream stream, MonadIO (stream IO)) =>  Logger -> EventStore.Connection -> Snapshot -> stream IO (Either PersistenceFailure PersistResult)
persist logger eventStoreConnection snapshot =  do

    eventIdInEventStoreDomain <- liftIO $ Uuid.nextRandom

    let eventType  = EventStore.UserDefined $ Text.pack $  "workspaceState"
        eventId = Just eventIdInEventStoreDomain
        eventData = EventStore.withJson snapshot
        eventInEventStoreDomain = EventStore.createEvent eventType eventId eventData
    writeResult <- liftIO $ EventStore.sendEvent
            eventStoreConnection
            (getStreamName $ workspaceId $ state snapshot)
            EventStore.anyVersion
            eventInEventStoreDomain
            getCredentials >>= wait

    liftIO $ logInfo logger $ "snapshot updated for workspace " ++ (show $ workspaceId $ state snapshot)
    S.yield $ Right $ PersistResult $ toInteger $ EventStore.writeNextExpectedVersion writeResult


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