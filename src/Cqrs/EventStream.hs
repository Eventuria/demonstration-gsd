{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Cqrs.EventStream (
readForward,
persist) where

import Streamly
import qualified Streamly.Prelude as S
import Control.Concurrent
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function ((&))
import Cqrs.EventStore
import qualified Database.EventStore as EventStore
import Cqrs.Core
import Cqrs.Events
import Cqrs.Streams
import Cqrs.Logger
import Control.Concurrent.Async (wait)

import qualified Data.Text as Text
import Data.UUID
import Data.Time
import qualified Data.UUID.V4 as Uuid
import Data.Maybe

import Data.Aeson

data PersistedEvent = PersistedEvent {
                                offset :: Offset ,
                                event :: Event}


instance Show PersistedEvent where
  show PersistedEvent { offset = offset , event = Event { eventHeader = commandHeader }} =
    "PersistedEvent { offset = " ++ ( show $ offset) ++ " , event = " ++ (show $ eventName commandHeader) ++ ":"
    ++ (show $ aggregateId commandHeader) ++ " }"

persist :: Logger -> EventStore.Connection -> Event -> IO (Either PersistenceFailure PersistResult)
persist logger eventStoreConnection event =  do

    eventIdInEventStoreDomain <- liftIO $ Uuid.nextRandom
    let eventType  = EventStore.UserDefined $ Text.pack $ eventName $ eventHeader event
        eventData = EventStore.withJson event
        eventInEventStoreDomain = EventStore.createEvent eventType (Just eventIdInEventStoreDomain) eventData
    writeResult <- liftIO $ EventStore.sendEvent
            eventStoreConnection
            (getEventStreamName $ aggregateId $ eventHeader event)
            EventStore.anyVersion
            eventInEventStoreDomain
            getCredentials >>= wait

    liftIO $ logInfo logger $ "Event " ++ (eventName $ eventHeader event) ++ " : id " ++ (toString $ eventId $ eventHeader event) ++ " persisted"
    return $ Right $ PersistResult $ toInteger $ EventStore.writeNextExpectedVersion writeResult

readForward :: (IsStream stream, MonadIO (stream IO), Semigroup (stream IO PersistedEvent)) => EventStore.Connection -> AggregateId -> Offset -> stream IO PersistedEvent
readForward eventStoreConnection  workSpaceId fromOffset = do
               let batchSize = 100 :: Integer
                   resolveLinkTos = False
               asyncRead <- liftIO $ EventStore.readStreamEventsForward
                                eventStoreConnection
                                (getEventStreamName workSpaceId)
                                (fromInteger fromOffset)
                                (fromInteger batchSize)
                                resolveLinkTos
                                getCredentials
               eventFetched <- liftIO $ wait asyncRead
               case eventFetched of
                    EventStore.ReadSuccess readResult -> do
                        let persistedEvents = convertJsonToPersistedEvents readResult
                        if (length persistedEvents) /= 0 then do
                            (S.fromList persistedEvents) <> (readForward eventStoreConnection workSpaceId $ fromOffset + batchSize)
                        else S.fromList persistedEvents
                    e -> error $ "Read failure: " <> show e

convertJsonToPersistedEvent :: Offset -> EventStore.ResolvedEvent ->  PersistedEvent
convertJsonToPersistedEvent offset eventData  = PersistedEvent { offset = offset, event = fromJust $ EventStore.resolvedEventDataAsJson eventData }

convertJsonToPersistedEvents :: EventStore.StreamSlice -> [PersistedEvent]
convertJsonToPersistedEvents eventSlice = (\event -> convertJsonToPersistedEvent (toInteger $ EventStore.recordedEventNumber $ EventStore.resolvedEventOriginal $ event) event)
                                                                                          <$> EventStore.sliceEvents eventSlice

getEventStreamName :: AggregateId -> EventStore.StreamName
getEventStreamName workspaceId = EventStore.StreamName $ Text.pack $ "workspace_event-" ++ toString workspaceId

