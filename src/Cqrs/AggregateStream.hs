{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Cqrs.AggregateStream (
PersistedAggregate(..),
streamAllInfinitely,
streamAll,
streamFromOffset,
subscribeToNewAggregateCreated,
subscribeToAggregateUpdates,
yieldAndSubscribeToAggregateUpdates

) where

import qualified Cqrs.CommandStream as CommandStream
import Prelude hiding (catch)
import Cqrs.EventStore
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

import Streamly
import qualified Streamly.Prelude as S
import Control.Concurrent
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Function ((&))
import Data.Either
type CorruptedStreamName = String

data PersistedAggregate = PersistedAggregate { offset :: Offset, aggregateIdPersisted :: AggregateId } deriving (Show,Eq)

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
                              (S.yield $ getCreatedAggregate $ (EventStore.resolvedEventOriginal resolvedEvent)) <> loopNextEvent subscription

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
                EventStore.Connection ->
                stream IO PersistedAggregate
streamAll logger eventStoreConnection = streamFromOffset logger eventStoreConnection 0

streamAllInfinitely ::  (IsStream stream,
                         MonadIO (stream IO),
                         Semigroup (stream IO PersistedAggregate)) =>
                          Logger ->
                          EventStore.Connection ->
                          stream IO PersistedAggregate
streamAllInfinitely  logger eventStoreConnection =
  (subscribeToNewAggregateCreated logger eventStoreConnection)
    `parallel` (streamAll logger eventStoreConnection)


streamFromOffset :: (IsStream stream,
                     MonadIO (stream IO),
                     Semigroup (stream IO PersistedAggregate)) =>
                      Logger ->
                      EventStore.Connection ->
                      Offset ->
                      stream IO PersistedAggregate
streamFromOffset logger eventStoreConnection fromOffset = do
               liftIO $ logInfo logger $ "starting streaming commands from offset " ++ (show fromOffset)
               let batchSize = 100 :: Integer
                   resolveLinkTos = False
               asyncRead <- liftIO $ EventStore.readStreamEventsForward
                                eventStoreConnection
                                getAggregateCreatedStreamName
                                (fromInteger fromOffset)
                                (fromInteger batchSize)
                                resolveLinkTos
                                getCredentials
               commandFetched <- liftIO $ wait asyncRead
               case commandFetched of
                    EventStore.ReadSuccess readResult -> do
                        let commandStreamNamesByAggregateId = getCreatedAggregate
                                                              <$> EventStore.resolvedEventOriginal
                                                              <$> EventStore.sliceEvents readResult
                        if (length commandStreamNamesByAggregateId) /= 0 then do
                            (S.fromList commandStreamNamesByAggregateId) <>
                              (streamFromOffset logger eventStoreConnection $ fromOffset + batchSize)
                        else S.fromList commandStreamNamesByAggregateId
                    EventStore.ReadNoStream -> error $ "ReadNoStream "
                    e -> error $ "Read failure: " <> show e

getCreatedAggregate :: EventStore.RecordedEvent -> PersistedAggregate
getCreatedAggregate recordedEvent =
  PersistedAggregate { offset = toInteger $ EventStore.recordedEventNumber recordedEvent,
                       aggregateIdPersisted = case (fromString $ drop (length  ("0@aggregate_command-" :: [Char])) $ unpack $ EventStore.recordedEventData recordedEvent) of
                                          Just aggregateId -> aggregateId
                                          Nothing -> error $ "Aggregate stream corrupted :" ++ (unpack $ EventStore.recordedEventData recordedEvent) }

