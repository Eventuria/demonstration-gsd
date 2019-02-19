{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Write.Repository.EventStoreStreams  where

import qualified Data.Text as Text
import Database.EventStore hiding (Command)
import Data.UUID

import PersistedStreamEngine.Instances.EventStore.Client.Dependencies
import PersistedStreamEngine.Instances.EventStore.EventStoreStream
import CQRS.Write.StreamRepository
import CQRS.Write.Aggregate.Ids.AggregateId
import Gsd.Write.Model.State

getEventStoreStreamRepository :: Dependencies -> CqrsStreamRepository EventStoreStream GsdState
getEventStoreStreamRepository dependencies =
  CqrsStreamRepository  {
    aggregateIdStream =                        getAggregateStream    dependencies "gsd_aggregate_id",
    getCommandStream  =        \aggregateId -> getAggregateSubStream dependencies "gsd_aggregate_commands-"          aggregateId,
    getCommandResponseStream = \aggregateId -> getAggregateSubStream dependencies "gsd_aggregate_command_response_-" aggregateId,
    getValidationStateStream = \aggregateId -> getAggregateSubStream dependencies "gsd_aggregate_validation_states-" aggregateId,
    getEventStream =           \aggregateId -> getAggregateSubStream dependencies "gsd_aggregate_events-"            aggregateId}
  where
      getAggregateStream :: Dependencies -> String ->  EventStoreStream item
      getAggregateStream dependencies streamName =
          EventStoreStream {dependencies,
                            streamName = StreamName $ Text.pack $ streamName }

      getAggregateSubStream :: Dependencies -> String -> AggregateId -> EventStoreStream item
      getAggregateSubStream context streamNameBase aggregateId =
          EventStoreStream { dependencies,
                             streamName = StreamName $ Text.pack $ streamNameBase ++ (toString $ aggregateId)}