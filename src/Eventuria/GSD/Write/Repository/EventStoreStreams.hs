{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.GSD.Write.Repository.EventStoreStreams  where

import qualified Data.Text as Text
import           Data.UUID

import           Database.EventStore hiding (Command)

import           Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Dependencies
import           Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.EventStoreStream

import           Eventuria.Libraries.CQRS.Write.StreamRepository
import           Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId

import           Eventuria.GSD.Write.Model.State


getEventStoreStreamRepository :: Dependencies -> CQRSStreamRepository EventStoreStream GsdState
getEventStoreStreamRepository clientDependencies =
  CQRSStreamRepository  {
    aggregateIdStream =                        getAggregateStream    clientDependencies "gsd_aggregate_id",
    getCommandStream  =        \aggregateId -> getAggregateSubStream clientDependencies "gsd_aggregate_commands-"          aggregateId,
    getCommandResponseStream = \aggregateId -> getAggregateSubStream clientDependencies "gsd_aggregate_command_response_-" aggregateId,
    getValidationStateStream = \aggregateId -> getAggregateSubStream clientDependencies "gsd_aggregate_validation_states-" aggregateId,
    getEventStream =           \aggregateId -> getAggregateSubStream clientDependencies "gsd_aggregate_events-"            aggregateId}
  where
      getAggregateStream :: Dependencies -> String ->  EventStoreStream item
      getAggregateStream clientDependencies streamName =
          EventStoreStream {clientDependencies,
                            streamName = StreamName $ Text.pack $ streamName }

      getAggregateSubStream :: Dependencies -> String -> AggregateId -> EventStoreStream item
      getAggregateSubStream clientDependencies streamNameBase aggregateId =
          EventStoreStream { clientDependencies,
                             streamName = StreamName $ Text.pack $ streamNameBase ++ (toString $ aggregateId)}