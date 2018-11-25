{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Cqrs.Aggregate.Commands.CommandStream  where

import Cqrs.Aggregate.Commands.Command
import Control.Concurrent.Async (wait)
import qualified Database.EventStore as EventStore
import qualified Data.Text as Text
import Data.UUID
import Cqrs.Aggregate.Ids.AggregateId
import Data.Maybe
import Cqrs.Aggregate.Commands.PersistedCommand
import Streamly
import qualified Streamly.Prelude as S

import Control.Monad.IO.Class (MonadIO(liftIO))

import Cqrs.Streams

import Cqrs.EventStore.Stream
import Cqrs.EventStore.Context
import qualified Cqrs.EventStore.Writing as EventStore.Writing

type CommandStream = EventStoreStream PersistedCommand

getCommandStream :: EventStoreContext -> AggregateId -> CommandStream
getCommandStream context aggregateId = EventStoreStream {
                                                        context = context,
                                                        streamName = (getStreamName aggregateId),
                                                        recordedEventToPersistedItem = recordedEventToPersistedCommand }


recordedEventToPersistedCommand :: EventStore.RecordedEvent -> PersistedCommand
recordedEventToPersistedCommand recordedEvent =
  PersistedCommand { offset = toInteger $ EventStore.recordedEventNumber recordedEvent,
                     command = fromJust $ EventStore.recordedEventDataAsJson recordedEvent }


persist :: EventStoreContext -> Command -> IO (Either PersistenceFailure PersistResult)
persist context command @ Command { commandHeader = CommandHeader {commandId = commandId , aggregateId = aggregateId , commandName = commandName} , payload = payload } =
  EventStore.Writing.persist context (getStreamName aggregateId) command


readForward :: (IsStream stream, MonadIO (stream IO),Semigroup (stream IO PersistedCommand)) => EventStore.Credentials -> EventStore.Connection -> AggregateId -> Maybe Offset -> stream IO PersistedCommand
readForward credentials eventStoreConnection workspaceId fromOffset =  do
               let batchSize = 100 :: Integer
                   resolveLinkTos = False

               asyncRead <- liftIO $ EventStore.readStreamEventsForward
                                eventStoreConnection
                                (getStreamName workspaceId)
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

getStreamName :: AggregateId -> EventStore.StreamName
getStreamName aggregateId = EventStore.StreamName $ Text.pack $ "aggregate_command-" ++ toString aggregateId

instance EventStore.Writing.Persistable Command where
  getItemName Command { commandHeader = CommandHeader {commandName = commandName} }  = commandName