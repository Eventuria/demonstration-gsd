{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Gsd.Gsd (requestCommand,
                streamCommands,
                streamWorkspaceIds,
                getEventStoreStreamRepository,
                runCommandConsumers) where

import qualified Data.Text as Text
import Database.EventStore hiding (Command)
import Data.Maybe
import Data.UUID

import Data.Function ((&))
import qualified Streamly.Prelude as S

import Streamly.Streamable

import Logger.Core

import qualified Cqrs.CommandConsumerFlow as CommandConsumerFlow
import qualified Cqrs.Cqrs as Cqrs
import Cqrs.Aggregate.StreamRepository
import Cqrs.PersistedStream.Stream
import Cqrs.Streams
import Cqrs.Aggregate.Ids.AggregateId
import Cqrs.PersistedStream.PersistedItem
import Cqrs.Aggregate.Commands.Command

import Gsd.CommandHandler
import Gsd.Commands
import Gsd.Core

-- to be removed

import Plugins.GregYoungEventStore.Read.Streaming
import Plugins.GregYoungEventStore.Settings
import Plugins.GregYoungEventStore.Stream
import Plugins.GregYoungEventStore.InterpreterEventStore

requestCommand :: GetCommandStream -> AggregateIdStream -> GsdCommand -> IO (Either PersistenceFailure PersistResult)
requestCommand getCommandStream aggregateIdStream gsdCommand = Cqrs.persistCommands getCommandStream aggregateIdStream $ toCommand gsdCommand

runCommandConsumers :: Logger -> EventStoreContext -> Reading -> IO ()
runCommandConsumers logger eventStoreContext eventStoreReading = CommandConsumerFlow.runCommandConsumers logger (getEventStoreStreamRepository eventStoreContext) eventStoreReading gsdCommandHandler interpretWriteEventStoreLanguage

streamWorkspaceIds :: Streamable stream monad WorkspaceId => AggregateIdStream -> stream monad (Persisted WorkspaceId)
streamWorkspaceIds = streamAll


streamCommands ::  Streamable stream monad Command => GetCommandStream -> WorkspaceId -> stream monad (Persisted GsdCommand)
streamCommands getCommandStream workspaceId = do
  (streamAll (getCommandStream workspaceId) &
      S.map (\PersistedItem { offset = offset, item = cqrsCommand} ->
              PersistedItem { offset = offset, item = fromJust $ fromCommand $ cqrsCommand}))


getEventStoreStreamRepository :: EventStoreContext -> EventStoreStreamRepository
getEventStoreStreamRepository context = StreamRepository {
                                             aggregateIdStream =                        getAggregateStream    context "gsd_aggregate_id",
                                             getCommandStream  =        \aggregateId -> getAggregateSubStream context "gsd_aggregate_commands-"          aggregateId,
                                             getCommandResponseStream = \aggregateId -> getAggregateSubStream context "gsd_aggregate_command_response_-" aggregateId,
                                             getValidationStateStream = \aggregateId -> getAggregateSubStream context "gsd_aggregate_validation_states-" aggregateId,
                                             getEventStream =           \aggregateId -> getAggregateSubStream context "gsd_aggregate_events-"            aggregateId
                                           }

getAggregateStream :: EventStoreContext -> String ->  EventStoreStream item
getAggregateStream context streamName = EventStoreStream {context = context, streamName = StreamName $ Text.pack $ streamName }

getAggregateSubStream :: EventStoreContext -> String -> AggregateId -> EventStoreStream item
getAggregateSubStream context streamNameBase aggregateId = EventStoreStream {context = context,
                                                                     streamName = StreamName $ Text.pack $ streamNameBase ++ (toString $ aggregateId)}