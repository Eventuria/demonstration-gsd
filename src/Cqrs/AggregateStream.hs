{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Cqrs.AggregateStream  where

import qualified Cqrs.CommandStream as CommandStream
import Prelude hiding (catch)

import Cqrs.Command
import qualified Cqrs.Command as MyCommand
import Cqrs.Events
import Data.Time
import Data.Maybe
import qualified Data.UUID.V4 as Uuid
import Control.Concurrent.Async (wait)

import Data.Set (Set)
import qualified Data.Set as Set
import Cqrs.Core
import qualified Database.EventStore as EventStore
import Cqrs.Logger
import qualified Data.Text as Text
import Data.UUID
import Data.ByteString.Char8 as Char8 (unpack)
import qualified Control.Concurrent as Concurrent
import Control.Exception
import Cqrs.Streams
import Cqrs.PersistedAggregate

import Streamly
import qualified Streamly.Prelude as S
import Control.Concurrent
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Function ((&))
import Data.Either

import qualified Cqrs.EventStore.Stream as StreamGeneric

type CorruptedStreamName = String


getAggregateCreatedStreamName :: EventStore.StreamName
getAggregateCreatedStreamName = EventStore.StreamName $ Text.pack $ "$et-createWorkspace"


yieldAndSubscribeToAggregateUpdates :: (IsStream stream,
                                        MonadIO (stream IO),
                                        Semigroup (stream IO PersistedAggregate),
                                        Semigroup (stream IO (CommandStream.PersistedCommand)))  =>
                                       Logger -> EventStore.Connection -> PersistedAggregate -> stream IO PersistedAggregate
yieldAndSubscribeToAggregateUpdates logger eventStoreConnection aggregatePersisted =
  (S.yield aggregatePersisted) <>
  ((subscribeToAggregateUpdates logger eventStoreConnection $ aggregateIdPersisted aggregatePersisted )
    & S.map (\aggregateId -> PersistedAggregate { offset = offset aggregatePersisted ,
                                                     aggregateIdPersisted = aggregateId }))


subscribeToAggregateUpdates :: (IsStream stream,
                                MonadIO (stream IO),
                                Semigroup (stream IO (CommandStream.PersistedCommand)))  =>
                                  Logger ->
                                  EventStore.Connection ->
                                  AggregateId -> stream IO AggregateId
subscribeToAggregateUpdates logger eventStoreConnection aggregateId =
  ((CommandStream.subscribeToNewCommand logger eventStoreConnection aggregateId)
    & S.map (\persistedCommand -> aggregateId))

subscribeToNewAggregateCreated :: (IsStream stream,
                                   MonadIO (stream IO),
                                   Semigroup (stream IO PersistedAggregate))  =>
                                    Logger ->
                                    EventStore.Connection ->
                                    stream IO PersistedAggregate
subscribeToNewAggregateCreated logger eventStoreConnection = do
              liftIO $ logInfo logger "subscribing to new aggregate created notifier"
              subscription <- liftIO $ EventStore.subscribe eventStoreConnection getAggregateCreatedStreamName True Nothing
              result <- liftIO $ (try $ EventStore.waitConfirmation subscription )
              case result of
                Left e @ SomeException {} -> do
                           liftIO $ logInfo logger "subscription to new aggregate created stream failed!"
                           liftIO $ threadDelay (5 * 1000000) -- 5 seconds
                           subscribeToNewAggregateCreated logger eventStoreConnection
                Right _ -> do
                           liftIO $ logInfo logger "subscription started for commands handlers"
                           loopNextEvent subscription where
                           loopNextEvent subscription = do
                              resolvedEvent <- liftIO $ EventStore.nextEvent subscription
                              liftIO $ logInfo logger $ "new aggregate created stream event triggered : " ++ (show resolvedEvent)
                              (S.yield $ recordedEventToPersistedAggregate $ (EventStore.resolvedEventOriginal resolvedEvent)) <> loopNextEvent subscription

waitSubscriptionConfirmation :: EventStore.Subscription s => s -> IO (Either SomeException ())
waitSubscriptionConfirmation subscription  = do
               result <- try $ EventStore.waitConfirmation subscription
               return $ case result of
                 Left e -> Left e
                 Right _ -> Right ()

streamAll :: (IsStream stream,
              MonadIO (stream IO),
              Semigroup (stream IO PersistedAggregate)) =>
                Logger ->
                EventStore.Credentials ->
                EventStore.Connection ->
                stream IO PersistedAggregate
streamAll logger credentials eventStoreConnection = streamFromOffset logger credentials eventStoreConnection 0

streamAllInfinitely ::  (IsStream stream,
                         MonadIO (stream IO),
                         Semigroup (stream IO PersistedAggregate)) =>
                          Logger ->
                          EventStore.Credentials ->
                          EventStore.Connection ->
                          stream IO PersistedAggregate
streamAllInfinitely  logger credentials eventStoreConnection =
  (subscribeToNewAggregateCreated logger eventStoreConnection)
    `parallel` (streamAll logger credentials eventStoreConnection)


streamFromOffset :: (IsStream stream,
                     MonadIO (stream IO),
                     Semigroup (stream IO PersistedAggregate)) =>
                      Logger ->
                      EventStore.Credentials ->
                      EventStore.Connection ->
                      Offset ->
                      stream IO PersistedAggregate
streamFromOffset logger credentials eventStoreConnection fromOffset =
  StreamGeneric.streamFromOffset logger credentials getAggregateCreatedStreamName eventStoreConnection recordedEventToPersistedAggregate fromOffset

recordedEventToPersistedAggregate :: EventStore.RecordedEvent -> PersistedAggregate
recordedEventToPersistedAggregate recordedEvent =
  PersistedAggregate { offset = toInteger $ EventStore.recordedEventNumber recordedEvent,
                       aggregateIdPersisted = case (fromString $ drop (length  ("0@aggregate_command-" :: [Char])) $ unpack $ EventStore.recordedEventData recordedEvent) of
                                          Just aggregateId -> aggregateId
                                          Nothing -> error $ "Aggregate stream corrupted :" ++ (unpack $ EventStore.recordedEventData recordedEvent) }

