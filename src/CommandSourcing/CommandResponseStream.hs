{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module CommandSourcing.CommandResponseStream (
readForward,
persist) where

import CommandSourcing.Core
import CommandSourcing.CommandResponse
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


data PersistenceFailure = CommandAlreadyPersisted
data PersistResult = PersistResult {writeNextVersion :: Integer}

persist :: ConduitT (Connection,CommandResponse) (Either PersistenceFailure PersistResult) IO()
persist = awaitForever $ \(eventStoreConnection , commandResponse) ->  do

    let logger = "[gsd.persist.command.request]"
    liftIO $ updateGlobalLogger logger $ setLevel INFO

    eventIdInEventStoreDomain <- liftIO $ Uuid.nextRandom

    let credentials = Nothing
        eventType  = UserDefined $ Text.pack $ serializedCommandResponseName commandResponse
        eventId = Just eventIdInEventStoreDomain
        eventData = withJson commandResponse
        eventInEventStoreDomain = createEvent eventType eventId eventData
    writeResult <- liftIO $ sendEvent
            eventStoreConnection
            (getWorkspaceCommandResponseStreamName $ workspaceId commandResponse)
            anyVersion
            eventInEventStoreDomain
            credentials >>= wait

    liftIO $ infoM logger "command Response request persisted"
    yield $ Right $ PersistResult $ toInteger $ writeNextExpectedVersion writeResult

readForward :: ConduitT (Connection,WorkspaceId,Offset) CommandResponse IO()
readForward = awaitForever $ \(eventStoreConnection , workspaceId, fromOffset) -> do
               let credentials = Nothing
                   batchSize = 100 :: Integer
                   resolveLinkTos = False
               asyncRead <- liftIO $ readStreamEventsForward
                                eventStoreConnection
                                (getWorkspaceCommandResponseStreamName workspaceId)
                                (fromInteger fromOffset)
                                (fromInteger batchSize)
                                resolveLinkTos
                                credentials
               commandFetched <- liftIO $ wait asyncRead
               case commandFetched of
                    ReadSuccess readResult -> do
                        let commandResponses = getCommandResponseRequestFromReadResult readResult
                        if (length commandResponses) /= 0 then do
                            yieldMany commandResponses
                            leftover (eventStoreConnection , workspaceId, fromOffset + batchSize)
                            CommandSourcing.CommandResponseStream.readForward
                        else yieldMany $ commandResponses
                    e -> error $ "Read failure: " <> show e


getCommandResponseRequestFromReadResult :: StreamSlice -> [CommandResponse]
getCommandResponseRequestFromReadResult sl = catMaybes $ resolvedEventDataAsJson <$> sliceEvents sl

getWorkspaceCommandResponseStreamName :: WorkspaceId -> StreamName
getWorkspaceCommandResponseStreamName workspaceId = StreamName $ Text.pack $ "workspace_response_command-" ++ toString workspaceId


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
