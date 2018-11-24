{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Cqrs.Commands.CommandStream  where

import Cqrs.Logger
import Cqrs.Commands.Command
import Control.Concurrent.Async (wait)
import qualified Database.EventStore as EventStore
import qualified Data.Text as Text
import Data.UUID
import Cqrs.Aggregate.Ids.AggregateId
import qualified Data.UUID.V4 as Uuid
import Data.Maybe
import Cqrs.Commands.PersistedCommand
import Data.Aeson
import Streamly
import qualified Streamly.Prelude as S

import Control.Monad.IO.Class (MonadIO(liftIO))

import Cqrs.Streams

import Cqrs.EventStore.Stream
import Cqrs.EventStore.Context

type CommandStream = EventStoreStream PersistedCommand

getCommandStream :: EventStoreContext -> AggregateId -> CommandStream
getCommandStream context aggregateId = EventStoreStream {
                                                        context = context,
                                                        streamName = (getWorkspaceCommandStreamName aggregateId),
                                                        recordedEventToPersistedItem = recordedEventToPersistedCommand }


recordedEventToPersistedCommand :: EventStore.RecordedEvent -> PersistedCommand
recordedEventToPersistedCommand recordedEvent =
  PersistedCommand { offset = toInteger $ EventStore.recordedEventNumber recordedEvent,
                     command = fromJust $ EventStore.recordedEventDataAsJson recordedEvent }


persist :: (IsStream stream, MonadIO (stream IO)) => Logger -> EventStore.Credentials -> EventStore.Connection -> Command -> stream IO (Either PersistenceFailure PersistResult)
persist logger credentials eventStoreConnection Command { commandHeader = CommandHeader {commandId = commandId , aggregateId = aggregateId , commandName = commandName} , payload = payload } =  do

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
            (Just credentials) >>= wait

    liftIO $ logInfo logger $ "Command " ++ commandName ++ " : command id " ++ (toString $ commandId) ++ " persisted"
    S.yield $ Right $ PersistResult $ toInteger $ EventStore.writeNextExpectedVersion writeResult

readForward :: (IsStream stream, MonadIO (stream IO),Semigroup (stream IO PersistedCommand)) => EventStore.Credentials -> EventStore.Connection -> AggregateId -> Maybe Offset -> stream IO PersistedCommand
readForward credentials eventStoreConnection workspaceId fromOffset =  do
               let batchSize = 100 :: Integer
                   resolveLinkTos = False

               asyncRead <- liftIO $ EventStore.readStreamEventsForward
                                eventStoreConnection
                                (getWorkspaceCommandStreamName workspaceId)
                                (fromInteger $ fromMaybe 0 fromOffset)
                                (fromInteger batchSize)
                                resolveLinkTos
                                (Just credentials)
               commandFetched <- liftIO $ wait asyncRead
               case commandFetched of
                    EventStore.ReadSuccess readResult -> do
                        let persistedCommands = getPersistedCommandRequestFrom readResult
                        if (length persistedCommands) /= 0 then do
                            (S.fromList persistedCommands) <> (readForward credentials eventStoreConnection workspaceId $ ( (+ batchSize) <$> Just (fromMaybe 0 fromOffset)))
                        else S.fromList $ persistedCommands
                    e -> error $ "Read failure: " <> show e



getPersistedCommandRequestFrom :: EventStore.StreamSlice -> [PersistedCommand]
getPersistedCommandRequestFrom eventSlice = (\event -> recordedEventToPersistedCommand $ EventStore.resolvedEventOriginal $ event)
                                                        <$> EventStore.sliceEvents eventSlice

getWorkspaceCommandStreamName :: AggregateId -> EventStore.StreamName
getWorkspaceCommandStreamName workspaceId = EventStore.StreamName $ Text.pack $ "workspace_command-" ++ toString workspaceId

