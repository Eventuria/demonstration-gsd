{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
module Cqrs.Aggregate.Snapshots.AggregateSnapshotStream where

import Control.Monad.IO.Class (MonadIO(..))

import Cqrs.Logger
import Cqrs.Aggregate.Snapshots.AggregateSnapshot
import Control.Concurrent.Async (wait)
import qualified Database.EventStore as EventStore
import qualified Data.Text as Text
import Data.UUID

import Cqrs.Aggregate.Snapshots.PersistedAggregateSnapshot
import qualified Data.UUID.V4 as Uuid
import Data.Maybe

import Cqrs.EventStore.Querying
import Cqrs.Streams
import Cqrs.EventStore.Stream
import Cqrs.EventStore.Context
import Cqrs.Aggregate.Ids.AggregateId

type AggregateSnapshotStream = EventStoreStream PersistedAggregateSnapshot



retrieveLastOffsetConsumed :: AggregateSnapshotStream -> IO (Maybe Offset)
retrieveLastOffsetConsumed aggregateSnapshotStream = (fmap.fmap) ( \persistedAggregateSnapshot -> lastOffsetConsumed $ aggregateSnapshot persistedAggregateSnapshot ) (retrieveLast aggregateSnapshotStream)


getAggregateSnapshotStream :: EventStoreContext -> AggregateId -> AggregateSnapshotStream
getAggregateSnapshotStream context aggregateId = EventStoreStream {
                                                        context = context,
                                                        streamName = (getStreamName aggregateId),
                                                        recordedEventToPersistedItem = recordedEventToPersistedAggregateSnapshot }

recordedEventToPersistedAggregateSnapshot :: EventStore.RecordedEvent -> PersistedAggregateSnapshot
recordedEventToPersistedAggregateSnapshot recordedEvent =
  PersistedAggregateSnapshot { offset = toInteger $ EventStore.recordedEventNumber recordedEvent,
                               aggregateSnapshot = fromJust $ EventStore.recordedEventDataAsJson recordedEvent }


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



