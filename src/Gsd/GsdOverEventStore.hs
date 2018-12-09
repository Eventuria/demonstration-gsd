{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Gsd.GsdOverEventStore (requestCommand,
                streamCommands,
                streamWorkspaceIds,
                runCommandConsumers) where

import qualified Data.Text as Text
import Database.EventStore hiding (Command)
import Data.UUID

import Streamly.Streamable

import Logger.Core

import Cqrs.PersistedStream.Repository
import Cqrs.Aggregate.Ids.AggregateId
import Cqrs.PersistedStream.PersistedItem
import Cqrs.Aggregate.Commands.Command
import Cqrs.PersistedStream.Write.PersistenceResult


import Gsd.Commands
import Gsd.Core

import qualified Gsd.GenericGsd as GenericGsd

import Plugins.GregYoungEventStore.EventStoreSettings
import Plugins.GregYoungEventStore.EventStoreStream
import Plugins.GregYoungEventStore.CqrsEDSLInterpreter

import Plugins.GregYoungEventStore.Read.CqrsInstance
import Plugins.GregYoungEventStore.Write.CqrsInstance


requestCommand ::  EventStoreSettings -> GsdCommand -> IO PersistenceResult
requestCommand settings gsdCommand =
  GenericGsd.requestCommand
    (getEventStoreStreamRepository settings)
    getEventStoreQuerying
    getEventStoreWriting
    gsdCommand

runCommandConsumers :: EventStoreSettings -> Logger ->  IO ()
runCommandConsumers settings logger  =
   GenericGsd.runCommandConsumers
      (getEventStoreStreamRepository settings)
      getEventStoreReading
      interpretWriteEventStoreLanguage
      logger


streamWorkspaceIds :: Streamable stream monad WorkspaceId => EventStoreSettings -> stream monad (Persisted WorkspaceId)
streamWorkspaceIds settings =
    GenericGsd.streamWorkspaceIds
      (getEventStoreStreamRepository settings)
      getEventStoreStreaming


streamCommands ::  Streamable stream monad Command => EventStoreSettings -> WorkspaceId -> stream monad (Persisted GsdCommand)
streamCommands settings workspaceId =
    GenericGsd.streamCommands
      (getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId


getEventStoreStreamRepository :: EventStoreSettings -> CqrsStreamRepository EventStoreStream
getEventStoreStreamRepository settings = CqrsStreamRepository  {
                                             aggregateIdStream =                        getAggregateStream    settings "gsd_aggregate_id",
                                             getCommandStream  =        \aggregateId -> getAggregateSubStream settings "gsd_aggregate_commands-"          aggregateId,
                                             getCommandResponseStream = \aggregateId -> getAggregateSubStream settings "gsd_aggregate_command_response_-" aggregateId,
                                             getValidationStateStream = \aggregateId -> getAggregateSubStream settings "gsd_aggregate_validation_states-" aggregateId,
                                             getEventStream =           \aggregateId -> getAggregateSubStream settings "gsd_aggregate_events-"            aggregateId
                                           }

getAggregateStream :: EventStoreSettings -> String ->  EventStoreStream item
getAggregateStream context streamName = EventStoreStream {settings = context, streamName = StreamName $ Text.pack $ streamName }

getAggregateSubStream :: EventStoreSettings -> String -> AggregateId -> EventStoreStream item
getAggregateSubStream context streamNameBase aggregateId = EventStoreStream {settings = context,
                                                                     streamName = StreamName $ Text.pack $ streamNameBase ++ (toString $ aggregateId)}