{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module CommandSourcing.CommandStream (
readForward,
persist,
subscribeToNewCommand) where

import CommandSourcing.Core
import CommandSourcing.Commands
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


data PersistenceFailure = CommandAlreadyPersisted
data PersistResult = PersistResult {writeNextVersion :: Integer}

subscribeToNewCommand :: EventStore.Connection -> ConduitT (WorkspaceId) Command IO()
subscribeToNewCommand eventStoreConnection = awaitForever $ \workSpaceId ->  do
              let logger = "[gsd.persist.command.request]"
              liftIO $ updateGlobalLogger logger $ setLevel INFO
              subscription <- liftIO $ EventStore.subscribe eventStoreConnection (getWorkspaceCommandStreamName workSpaceId) True Nothing
              _    <- liftIO $ EventStore.waitConfirmation subscription
              liftIO $ infoM logger "subscription started for commands handlers"
              event <- liftIO $ EventStore.nextEvent subscription
              liftIO $ infoM logger "event received"
              let commandMaybe = (EventStore.resolvedEventDataAsJson event) :: Maybe Command
              liftIO $ infoM logger $ "command received" ++ (show commandMaybe)
              case commandMaybe of
                Just command -> yield command


persist :: ConduitT (EventStore.Connection,Command) (Either PersistenceFailure PersistResult) IO()
persist = awaitForever $ \(eventStoreConnection , command) ->  do

    -- load command stream for workspace id provided in the command request
    -- create write model :
        -- List of all commandId from aggregate to avoid duplicates
        -- Malformed command request //
    let logger = "[gsd.persist.command.request]"
    liftIO $ updateGlobalLogger logger $ setLevel INFO

    eventIdInEventStoreDomain <- liftIO $ Uuid.nextRandom

    let credentials = getCre
        eventType  = EventStore.UserDefined $ Text.pack $ serializedCommandName command
        eventId = Just eventIdInEventStoreDomain
        eventData = EventStore.withJson command
        eventInEventStoreDomain = EventStore.createEvent eventType eventId eventData
    writeResult <- liftIO $ EventStore.sendEvent
            eventStoreConnection
            (getWorkspaceCommandStreamName $ workspaceId command)
            EventStore.anyVersion
            eventInEventStoreDomain
            credentials >>= wait

    liftIO $ infoM logger $ "[commandStream.persist] - command" ++ (show command)
    yield $ Right $ PersistResult $ toInteger $ EventStore.writeNextExpectedVersion writeResult

readForward :: ConduitT (EventStore.Connection,WorkspaceId,Offset) Command IO()
readForward = awaitForever $ \(eventStoreConnection , workspaceId, fromOffset) -> do
               let credentials = Nothing
                   batchSize = 100 :: Integer
                   resolveLinkTos = False
               asyncRead <- liftIO $ EventStore.readStreamEventsForward
                                eventStoreConnection
                                (getWorkspaceCommandStreamName workspaceId)
                                (fromInteger fromOffset)
                                (fromInteger batchSize)
                                resolveLinkTos
                                credentials
               commandFetched <- liftIO $ wait asyncRead
               case commandFetched of
                    EventStore.ReadSuccess readResult -> do
                        let commands = getCommandRequestFromResponse readResult
                        if (length commands) /= 0 then do
                            yieldMany commands
                            leftover (eventStoreConnection , workspaceId, fromOffset + batchSize)
                            CommandSourcing.CommandStream.readForward
                        else yieldMany $ commands
                    e -> error $ "Read failure: " <> show e


getCommandRequestFromResponse :: EventStore.StreamSlice -> [Command]
getCommandRequestFromResponse sl = catMaybes $ EventStore.resolvedEventDataAsJson <$> EventStore.sliceEvents sl

getWorkspaceCommandStreamName :: WorkspaceId -> EventStore.StreamName
getWorkspaceCommandStreamName workspaceId = EventStore.StreamName $ Text.pack $ "workspace_command-" ++ toString workspaceId

instance ToJSON PersistResult where
   toJSON (PersistResult writeNextVersion) = object [
             "writeNextVersion" .= writeNextVersion]


instance FromJSON PersistResult  where

    parseJSON (Object jsonObject) = PersistResult <$> jsonObject .: "writeNextVersion"


instance ToJSON PersistenceFailure where
   toJSON (CommandAlreadyPersisted) = object [("errorName" ,"CommandAlreadyPersisted")]


instance FromJSON PersistenceFailure  where

    parseJSON (Object jsonObject) = do
             errorNameMaybe <- jsonObject .: "errorName"
             case errorNameMaybe of
                  Just (String errorName) | (Text.unpack errorName) == "CommandAlreadyPersisted" -> return CommandAlreadyPersisted
                  Nothing -> error $ "error name not provided or invalid"
