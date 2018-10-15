{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module CommandSourcing.EventStream (
readForwardWorkspaceStream,
persist) where

import CommandSourcing.Core
import CommandSourcing.Events
import Control.Concurrent.Async (wait)
import Database.EventStore hiding (Command)
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

data PersistenceFailure = EventAlreadyPersisted
data PersistResult = PersistResult {writeNextVersion :: Integer}

persist :: ConduitT (Connection,WorkspaceEvent) (Either PersistenceFailure PersistResult) IO()
persist = awaitForever $ \(eventStoreConnection , workspaceEvent) ->  do

    let logger = "[gsd.persist.command.request]"
    liftIO $ updateGlobalLogger logger $ setLevel INFO

    eventIdInEventStoreDomain <- liftIO $ Uuid.nextRandom
    let credentials = Nothing
        eventType  = UserDefined $ Text.pack $ serializedEventName workspaceEvent
        eventId = Just eventIdInEventStoreDomain
        eventData = withJson workspaceEvent
        eventInEventStoreDomain = createEvent eventType eventId eventData
    writeResult <- liftIO $ sendEvent
            eventStoreConnection
            (getWorkspaceEventStreamName $ workspaceId workspaceEvent)
            anyVersion
            eventInEventStoreDomain
            credentials >>= wait

    liftIO $ infoM logger "Event persisted"
    yield $ Right $ PersistResult $ toInteger $ writeNextExpectedVersion writeResult

readForwardWorkspaceStream :: ConduitT (Connection,WorkspaceId,Offset) WorkspaceEvent IO()
readForwardWorkspaceStream = awaitForever $ \(eventStoreConnection , workSpaceId, fromOffset) -> do
               let credentials = Nothing
                   batchSize = 100 :: Integer
                   resolveLinkTos = False
               asyncRead <- liftIO $ readStreamEventsForward
                                eventStoreConnection
                                (getWorkspaceEventStreamName workSpaceId)
                                (fromInteger fromOffset)
                                (fromInteger batchSize)
                                resolveLinkTos
                                credentials
               eventFetched <- liftIO $ wait asyncRead
               case eventFetched of
                    ReadSuccess readResult -> do
                        let events = getEventsFromResponse readResult
                        if (length events) /= 0 then do
                            yieldMany events
                            leftover (eventStoreConnection , workSpaceId, fromOffset + batchSize)
                            CommandSourcing.EventStream.readForwardWorkspaceStream
                        else yieldMany $ events
                    e -> error $ "Read failure: " <> show e


getEventsFromResponse :: StreamSlice -> [WorkspaceEvent]
getEventsFromResponse sl = catMaybes $ resolvedEventDataAsJson <$> sliceEvents sl

getWorkspaceEventStreamName :: WorkspaceId -> StreamName
getWorkspaceEventStreamName workspaceId = StreamName $ Text.pack $ "workspace_event-" ++ toString workspaceId

instance ToJSON PersistResult where
   toJSON (PersistResult writeNextVersion) = object [
             "writeNextVersion" .= writeNextVersion]


instance FromJSON PersistResult  where

    parseJSON (Object jsonObject) = PersistResult <$> jsonObject .: "writeNextVersion"


instance ToJSON PersistenceFailure where
   toJSON (EventAlreadyPersisted) = object [("errorName" ,"EventAlreadyPersisted")]


instance FromJSON PersistenceFailure  where

    parseJSON (Object jsonObject) = do
             errorNameMaybe <- jsonObject .: "errorName"
             case errorNameMaybe of
                  Just (String errorName) | (Text.unpack errorName) == "EventAlreadyPersisted" -> return EventAlreadyPersisted
                  Nothing -> error $ "error name not provided or invalid"
