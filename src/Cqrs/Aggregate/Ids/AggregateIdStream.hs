{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Cqrs.Aggregate.Ids.AggregateIdStream  where

import Cqrs.Aggregate.Commands.CommandStream
import Prelude hiding (catch)


import Cqrs.Aggregate.Ids.AggregateId
import qualified Database.EventStore as EventStore
import qualified Data.Text as Text

import Streamly
import qualified Streamly.Prelude as S
import Control.Monad.IO.Class (MonadIO)
import Data.Function ((&))
import Cqrs.EventStore.Stream
import Cqrs.EventStore.Subscribing
import Cqrs.Aggregate.Commands.Command
import Cqrs.Aggregate.Core
import Cqrs.EventStore.Context
import Cqrs.EventStore.PersistedItem
import Cqrs.EventStore.Writing
import Cqrs.Streams
import Cqrs.EventStore.Streaming

type AggregateIdStream = EventStoreStream AggregateId

type CorruptedStreamName = String

getAggregateIdStream :: EventStoreContext -> AggregateIdStream
getAggregateIdStream eventStoreContext = EventStoreStream { context = eventStoreContext,
                                                          streamName = getAggregateIdStreamName}

getAggregateIdStreamName :: EventStore.StreamName
getAggregateIdStreamName = EventStore.StreamName $ Text.pack $ "aggregate_id"


persistCommands :: EventStoreContext -> Command -> IO (Either PersistenceFailure PersistResult)
persistCommands context command = do
  let commandStream = getCommandStream context $ getAggregateId command
  isStreamExist <- isStreamExistRequest commandStream
  if(isStreamExist) then
    persist commandStream command
  else do -- TODO : transaction !
    persist (getAggregateIdStream context) $ getAggregateId command
    persist commandStream command


instance Writable AggregateId where
  getItemName aggregateId  = "aggregateId"


yieldAndSubscribeToAggregateUpdates :: (IsStream stream,
                                        MonadIO (stream IO),
                                        Semigroup (stream IO (Persisted AggregateId)),
                                        Semigroup (stream IO (Persisted Command)))  =>
                                       EventStoreContext ->
                                       Persisted AggregateId ->
                                       stream IO (Persisted AggregateId)
yieldAndSubscribeToAggregateUpdates eventStoreContext persistedAggregate @ PersistedItem { offset = offset , item = aggregateId} =
  (S.yield persistedAggregate) <>
  ((subscribeToAggregateUpdates eventStoreContext aggregateId  ) & S.map (\aggregateId -> PersistedItem { offset = offset,item = aggregateId }))


subscribeToAggregateUpdates :: (IsStream stream,
                                MonadIO (stream IO),
                                Semigroup (stream IO (Persisted Command)))  =>
                                  EventStoreContext ->
                                  AggregateId ->
                                  stream IO AggregateId
subscribeToAggregateUpdates eventStoreContext aggregateId =
  (subscribe $ getCommandStream eventStoreContext aggregateId) & S.map (\persistedCommand -> aggregateId)



