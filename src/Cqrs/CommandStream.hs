{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Cqrs.CommandStream (
                          PersistedCommand (..),
                          readForward,
                          persist,
                          subscribeToNewCommand) where

import Cqrs.Logger
import Cqrs.EventStore
import Cqrs.Core
import Cqrs.Command
import Control.Concurrent.Async (wait)
import qualified Database.EventStore as EventStore
import qualified Data.Text as Text
import Data.UUID
import Data.Time

import qualified Data.UUID.V4 as Uuid
import Data.Maybe
import Cqrs.PersistedCommand
import Data.Aeson
import Streamly
import qualified Streamly.Prelude as S
import Control.Concurrent
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Function ((&))
import Cqrs.Streams



subscribeToWorkspaceUpdates :: (IsStream stream,
                                MonadIO (stream IO),
                                Semigroup (stream IO PersistedCommand ))  =>
                                  Logger ->
                                  EventStore.Connection ->
                                  AggregateId ->
                                  stream IO AggregateId
subscribeToWorkspaceUpdates logger eventStoreConnection aggregateIdGiven =
  ((subscribeToNewCommand logger eventStoreConnection aggregateIdGiven)
    & S.map (\persistedCommand -> aggregateId $ commandHeader $ command persistedCommand))


subscribeToNewCommand :: (IsStream stream,
                          MonadIO (stream IO),
                          Semigroup (stream IO PersistedCommand )) =>
                            Logger ->
                            EventStore.Connection ->
                            AggregateId -> stream IO PersistedCommand

subscribeToNewCommand logger eventStoreConnection workSpaceId = do
  subscription <- liftIO $ EventStore.subscribe eventStoreConnection (getWorkspaceCommandStreamName workSpaceId) True Nothing
  _    <- liftIO $ EventStore.waitConfirmation subscription
  liftIO $ logInfo logger $ "subscription started for commands from workspace " ++ (show workSpaceId)
  loopNextCommand subscription logger where
  loopNextCommand subscription logger = do
    event <- liftIO $ EventStore.nextEvent subscription
    let persistedCommand = convertJsonToPersistedCommand (toInteger $ EventStore.recordedEventNumber $ EventStore.resolvedEventOriginal event) event
    liftIO $ logInfo logger $ "command notified : " ++ (show persistedCommand)
    S.yield persistedCommand <> (loopNextCommand subscription logger)



convertJsonToPersistedCommand :: Offset -> EventStore.ResolvedEvent ->  PersistedCommand
convertJsonToPersistedCommand offset eventData  = PersistedCommand { offset = offset, command = fromJust $ EventStore.resolvedEventDataAsJson eventData }

persist :: (IsStream stream, MonadIO (stream IO)) => Logger -> EventStore.Connection -> Command -> stream IO (Either PersistenceFailure PersistResult)
persist logger eventStoreConnection Command { commandHeader = CommandHeader {commandId = commandId , aggregateId = aggregateId , commandName = commandName} , payload = payload } =  do

    eventIdInEventStoreDomain <- liftIO $ Uuid.nextRandom

    let eventType  = EventStore.UserDefined $ Text.pack $ commandName
        eventId = Just eventIdInEventStoreDomain
        eventData = EventStore.withJson $ object [
              "commandName" .= commandName,
              "commandId" .= commandId,
              "aggregateId" .= aggregateId,
              "payload" .= payload ]
        eventInEventStoreDomain = EventStore.createEvent eventType eventId eventData
    writeResult <- liftIO $ EventStore.sendEvent
            eventStoreConnection
            (getWorkspaceCommandStreamName $ aggregateId )
            EventStore.anyVersion
            eventInEventStoreDomain
            getCredentials >>= wait

    liftIO $ logInfo logger $ "Command " ++ commandName ++ " : command id " ++ (toString $ commandId) ++ " persisted"
    S.yield $ Right $ PersistResult $ toInteger $ EventStore.writeNextExpectedVersion writeResult

readForward :: (IsStream stream, MonadIO (stream IO),Semigroup (stream IO PersistedCommand)) => EventStore.Connection -> AggregateId -> Maybe Offset -> stream IO PersistedCommand
readForward eventStoreConnection workspaceId fromOffset =  do
               let batchSize = 100 :: Integer
                   resolveLinkTos = False

               asyncRead <- liftIO $ EventStore.readStreamEventsForward
                                eventStoreConnection
                                (getWorkspaceCommandStreamName workspaceId)
                                (fromInteger $ fromMaybe 0 fromOffset)
                                (fromInteger batchSize)
                                resolveLinkTos
                                getCredentials
               commandFetched <- liftIO $ wait asyncRead
               case commandFetched of
                    EventStore.ReadSuccess readResult -> do
                        let persistedCommands = getPersistedCommandRequestFrom readResult
                        if (length persistedCommands) /= 0 then do
                            (S.fromList persistedCommands) <> (readForward eventStoreConnection workspaceId $ ( (+ batchSize) <$> Just (fromMaybe 0 fromOffset)))
                        else S.fromList $ persistedCommands
                    e -> error $ "Read failure: " <> show e



getPersistedCommandRequestFrom :: EventStore.StreamSlice -> [PersistedCommand]
getPersistedCommandRequestFrom eventSlice = (\event -> convertJsonToPersistedCommand (toInteger $ EventStore.recordedEventNumber $ EventStore.resolvedEventOriginal $ event) event)
                                                        <$> EventStore.sliceEvents eventSlice

getWorkspaceCommandStreamName :: AggregateId -> EventStore.StreamName
getWorkspaceCommandStreamName workspaceId = EventStore.StreamName $ Text.pack $ "workspace_command-" ++ toString workspaceId

