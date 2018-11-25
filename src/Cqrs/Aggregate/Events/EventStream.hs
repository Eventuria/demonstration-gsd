{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Cqrs.Aggregate.Events.EventStream (
readForward,
persist) where

import Streamly
import qualified Streamly.Prelude as S
import Control.Monad.IO.Class (MonadIO(..))
import qualified Database.EventStore as EventStore
import Cqrs.Aggregate.Events.Event
import Cqrs.Streams

import Control.Concurrent.Async (wait)
import Cqrs.Aggregate.Ids.AggregateId
import Cqrs.EventStore.Context
import qualified Data.Text as Text
import Data.UUID
import Data.Maybe
import qualified Cqrs.EventStore.Writing as EventStore.Writing
data PersistedEvent = PersistedEvent {
                                offset :: Offset ,
                                event :: Event}

persist :: EventStoreContext -> Event -> IO (Either PersistenceFailure PersistResult)
persist context event @ Event { eventHeader = EventHeader { aggregateId = aggregateId}} = EventStore.Writing.persist context (getEventStreamName aggregateId) event

readForward :: (IsStream stream, MonadIO (stream IO), Semigroup (stream IO PersistedEvent)) => EventStore.Credentials -> EventStore.Connection -> AggregateId -> Offset -> stream IO PersistedEvent
readForward credentials eventStoreConnection  workSpaceId fromOffset = do
               let batchSize = 100 :: Integer
                   resolveLinkTos = False
               asyncRead <- liftIO $ EventStore.readStreamEventsForward
                                eventStoreConnection
                                (getEventStreamName workSpaceId)
                                (fromInteger fromOffset)
                                (fromInteger batchSize)
                                resolveLinkTos
                                (Just credentials)
               eventFetched <- liftIO $ wait asyncRead
               case eventFetched of
                    EventStore.ReadSuccess readResult -> do
                        let persistedEvents = convertJsonToPersistedEvents readResult
                        if (length persistedEvents) /= 0 then do
                            (S.fromList persistedEvents) <> (readForward credentials eventStoreConnection workSpaceId $ fromOffset + batchSize)
                        else S.fromList persistedEvents
                    e -> error $ "Read failure: " <> show e


convertJsonToPersistedEvents :: EventStore.StreamSlice -> [PersistedEvent]
convertJsonToPersistedEvents eventSlice = (\event -> convertJsonToPersistedEvent (toInteger $ EventStore.recordedEventNumber $ EventStore.resolvedEventOriginal $ event) event)
                                                                                          <$> EventStore.sliceEvents eventSlice
convertJsonToPersistedEvent :: Offset -> EventStore.ResolvedEvent ->  PersistedEvent
convertJsonToPersistedEvent offset eventData  = PersistedEvent { offset = offset, event = fromJust $ EventStore.resolvedEventDataAsJson eventData }

getEventStreamName :: AggregateId -> EventStore.StreamName
getEventStreamName workspaceId = EventStore.StreamName $ Text.pack $ "workspace_event-" ++ toString workspaceId

instance Show PersistedEvent where
  show PersistedEvent { offset = offset , event = Event { eventHeader = commandHeader }} =
    "PersistedEvent { offset = " ++ ( show $ offset) ++ " , event = " ++ (show $ eventName commandHeader) ++ ":"
    ++ (show $ aggregateId commandHeader) ++ " }"

instance EventStore.Writing.Persistable Event where
  getItemName Event { eventHeader = EventHeader { eventName = eventName}} = eventName
