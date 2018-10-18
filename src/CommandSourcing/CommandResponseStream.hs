{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module CommandSourcing.CommandResponseStream (
readForward,
persist) where

import Streamly
import qualified Streamly.Prelude as S
import Control.Concurrent
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function ((&))

import CommandSourcing.EventStore
import qualified Database.EventStore as EventStore

import CommandSourcing.EventStore
import CommandSourcing.Core
import CommandSourcing.CommandResponse
import CommandSourcing.Logger
import Control.Concurrent.Async (wait)
import qualified Data.Text as Text
import Data.UUID
import Data.Time

import qualified Data.UUID.V4 as Uuid
import Data.Maybe

import Data.Aeson
import CommandSourcing.Streams

persist :: (IsStream stream, MonadIO (stream IO)) => Logger -> EventStore.Connection -> CommandResponse -> stream IO (Either PersistenceFailure PersistResult)
persist logger eventStoreConnection commandResponse =  do

    eventIdInEventStoreDomain <- liftIO $ Uuid.nextRandom

    let eventType  = EventStore.UserDefined $ Text.pack $ getCommandResponseName commandResponse
        eventId = Just eventIdInEventStoreDomain
        eventData = EventStore.withJson commandResponse
        eventInEventStoreDomain = EventStore.createEvent eventType eventId eventData
    writeResult <- liftIO $ EventStore.sendEvent
            eventStoreConnection
            (getWorkspaceCommandResponseStreamName $ workspaceId commandResponse)
            EventStore.anyVersion
            eventInEventStoreDomain
            getCredentials >>= wait

    liftIO $ logInfo logger $ "Command Response " ++ (getCommandResponseName commandResponse) ++ " : command id " ++ (toString $ getCommandId commandResponse) ++ " persisted"
    S.yield $ Right $ PersistResult $ toInteger $ EventStore.writeNextExpectedVersion writeResult

readForward :: (IsStream stream, MonadIO (stream IO), Semigroup (stream IO CommandResponse)) => EventStore.Connection -> WorkspaceId -> Offset -> stream IO CommandResponse
readForward eventStoreConnection  workspaceId fromOffset =  do
               let batchSize = 100 :: Integer
                   resolveLinkTos = False
               asyncRead <- liftIO $ EventStore.readStreamEventsForward
                                eventStoreConnection
                                (getWorkspaceCommandResponseStreamName workspaceId)
                                (fromInteger fromOffset)
                                (fromInteger batchSize)
                                resolveLinkTos
                                getCredentials
               commandFetched <- liftIO $ wait asyncRead
               case commandFetched of
                    EventStore.ReadSuccess readResult -> do
                        let commandResponses = getCommandResponseRequestFromReadResult readResult
                        if (length commandResponses) /= 0 then do
                            (S.fromList commandResponses) <> (readForward eventStoreConnection workspaceId $ fromOffset + batchSize)
                        else S.fromList commandResponses
                    e -> error $ "Read failure: " <> show e


getCommandResponseRequestFromReadResult :: EventStore.StreamSlice -> [CommandResponse]
getCommandResponseRequestFromReadResult sl = catMaybes $ EventStore.resolvedEventDataAsJson <$> EventStore.sliceEvents sl

getWorkspaceCommandResponseStreamName :: WorkspaceId -> EventStore.StreamName
getWorkspaceCommandResponseStreamName workspaceId = EventStore.StreamName $ Text.pack $ "workspace_response_command-" ++ toString workspaceId
