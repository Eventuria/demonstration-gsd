{-# LANGUAGE OverloadedStrings #-}
module Gsd.Write.EventStoreStreamRepository  where

import qualified Data.Text as Text
import Database.EventStore hiding (Command)
import Data.UUID

import PersistedStreamEngine.Instances.EventStore.EventStoreClientState
import PersistedStreamEngine.Instances.EventStore.EventStoreStream
import Cqrs.Write.StreamRepository
import Cqrs.Write.Aggregate.Ids.AggregateId
import Gsd.Write.State

getEventStoreStreamRepository :: EventStoreClientState -> CqrsStreamRepository EventStoreStream GsdState
getEventStoreStreamRepository settings =
  CqrsStreamRepository  {
    aggregateIdStream =                        getAggregateStream    settings "gsd_aggregate_id",
    getCommandStream  =        \aggregateId -> getAggregateSubStream settings "gsd_aggregate_commands-"          aggregateId,
    getCommandResponseStream = \aggregateId -> getAggregateSubStream settings "gsd_aggregate_command_response_-" aggregateId,
    getValidationStateStream = \aggregateId -> getAggregateSubStream settings "gsd_aggregate_validation_states-" aggregateId,
    getEventStream =           \aggregateId -> getAggregateSubStream settings "gsd_aggregate_events-"            aggregateId}
  where
      getAggregateStream :: EventStoreClientState -> String ->  EventStoreStream item
      getAggregateStream context streamName = EventStoreStream {settings = context, streamName = StreamName $ Text.pack $ streamName }

      getAggregateSubStream :: EventStoreClientState -> String -> AggregateId -> EventStoreStream item
      getAggregateSubStream context streamNameBase aggregateId = EventStoreStream {settings = context,
                                                                     streamName = StreamName $ Text.pack $ streamNameBase ++ (toString $ aggregateId)}