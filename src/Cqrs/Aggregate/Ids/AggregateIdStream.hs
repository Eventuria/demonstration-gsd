{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
module Cqrs.Aggregate.Ids.AggregateIdStream  where


import Prelude hiding (catch)


import Cqrs.Aggregate.Ids.AggregateId


import qualified Streamly.Prelude as S
import Data.Function ((&))

import EventStore.Read.Subscribing
import Cqrs.Aggregate.Commands.Command


import EventStore.Read.PersistedItem

import EventStore.Streamable
import Cqrs.Aggregate.StreamRepository



yieldAndSubscribeToAggregateUpdates :: (Streamable monad stream Command, Streamable monad stream AggregateId) =>
                                       GetCommandStream ->
                                       Persisted AggregateId ->
                                       stream monad (Persisted AggregateId)
yieldAndSubscribeToAggregateUpdates getCommandStream persistedAggregate @ PersistedItem { offset = offset , item = aggregateId} =
  (S.yield persistedAggregate) <>
  ((subscribeToAggregateUpdates getCommandStream aggregateId  ) & S.map (\aggregateId -> PersistedItem { offset = offset,item = aggregateId }))


subscribeToAggregateUpdates :: (Streamable monad stream Command, Streamable monad stream AggregateId)  =>
                                  GetCommandStream -> AggregateId ->
                                  stream monad AggregateId
subscribeToAggregateUpdates getCommandStream aggregateId =
  (subscribe $ getCommandStream aggregateId) & S.map (\persistedCommand -> aggregateId)



