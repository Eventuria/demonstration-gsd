{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
module Cqrs.SnapshotStream (
retrieveLastOffsetConsumed,
retrieveLast,
persist
) where

import Streamly
import qualified Streamly.Prelude as S

import Control.Monad.IO.Class (MonadIO(..))
import Data.Function ((&))
import Control.Concurrent

import Cqrs.Logger
import Cqrs.Core
import Control.Concurrent.Async (wait)
import qualified Database.EventStore as EventStore
import qualified Data.Text as Text
import Data.UUID
import Data.Time

import qualified Data.UUID.V4 as Uuid
import Data.Maybe

import Data.Aeson
import Cqrs.Snapshot
import Cqrs.Streams
import Safe

retrieveLastOffsetConsumed :: EventStore.Credentials -> EventStore.Connection -> AggregateId -> IO (Maybe Offset)
retrieveLastOffsetConsumed credentials eventStoreConnection aggregateId =
  (fmap.fmap) lastOffsetConsumed (retrieveLast credentials eventStoreConnection aggregateId)

retrieveLast :: EventStore.Credentials -> EventStore.Connection -> AggregateId -> IO( Maybe AggregateSnapshot)
retrieveLast credentials eventStoreConnection aggregateId =  do
        let resolveLinkTos = False
        readResult <- EventStore.readStreamEventsBackward
                    eventStoreConnection
                    (getStreamName aggregateId)
                    (fromInteger 0)
                    (fromInteger 1)
                    resolveLinkTos
                    (Just credentials) >>= wait
        case readResult of
          EventStore.ReadSuccess responseContent -> do
              let snapshots = getSnapshotsFromResponse responseContent
              return $ listToMaybe snapshots
          EventStore.ReadNoStream -> return Nothing
          e -> error $ "Read failure: " <> show e

getSnapshotsFromResponse :: EventStore.StreamSlice -> [AggregateSnapshot]
getSnapshotsFromResponse sl = catMaybes $ EventStore.resolvedEventDataAsJson <$> EventStore.sliceEvents sl

persist :: Logger -> EventStore.Credentials -> EventStore.Connection -> AggregateSnapshot -> IO (Either PersistenceFailure PersistResult)
persist logger credentials eventStoreConnection snapshot =  do

    eventIdInEventStoreDomain <- liftIO $ Uuid.nextRandom

    let eventType  = EventStore.UserDefined $ Text.pack $  "workspaceState"
        eventId = Just eventIdInEventStoreDomain
        eventData = EventStore.withJson snapshot
        eventInEventStoreDomain = EventStore.createEvent eventType eventId eventData
    writeResult <- liftIO $ EventStore.sendEvent
            eventStoreConnection
            (getStreamName $ aggregateId $ state snapshot)
            EventStore.anyVersion
            eventInEventStoreDomain
            (Just credentials) >>= wait

    liftIO $ logInfo logger $ "snapshot updated for workspace " ++ (show $ aggregateId $ state snapshot)
    return $ Right $ PersistResult $ toInteger $ EventStore.writeNextExpectedVersion writeResult


getStreamName :: AggregateId -> EventStore.StreamName
getStreamName aggregateId = EventStore.StreamName $ Text.pack $ "workspace_snapshot-" ++ (toString aggregateId)



instance ToJSON AggregateState where
   toJSON (AggregateState aggregateId) = object ["aggregateId" .= aggregateId]

instance ToJSON AggregateSnapshot where
   toJSON (AggregateSnapshot lastOffsetConsumed commandsProcessed state) = object [
      "lastOffsetConsumed" .= lastOffsetConsumed,
      "commandsProcessed" .= commandsProcessed,
      "state" .= state
      ]

instance FromJSON AggregateSnapshot  where

    parseJSON (Object jsonObject) = AggregateSnapshot <$> jsonObject .: "lastOffsetConsumed"
                                             <*> jsonObject .: "commandsProcessed"
                                             <*> jsonObject .: "state"

instance FromJSON AggregateState  where

    parseJSON (Object jsonObject) = AggregateState <$> jsonObject .: "aggregateId"