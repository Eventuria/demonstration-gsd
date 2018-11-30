{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Gsd.Gsd (requestCommand,
                streamCommands,
                streamWorkspaceIds,
                getEventStoreStreamRepository,
                runCommandConsumers) where

import Gsd.Commands
import Gsd.Core
import qualified Cqrs.Cqrs as Cqrs
import Cqrs.Streams
import EventStore.Settings
import EventStore.Read.PersistedItem
import Cqrs.Aggregate.Commands.Command
import EventStore.Read.Streaming
import Data.Function ((&))
import qualified Streamly.Prelude as S
import Data.Maybe
import Data.UUID
import Cqrs.Aggregate.Ids.AggregateId
import EventStore.Streamable
import Cqrs.Aggregate.StreamRepository
import EventStore.Stream
import qualified Data.Text as Text
import Database.EventStore hiding (Command)
import qualified Cqrs.CommandConsumerFlow as CommandConsumerFlow
import Logger.Core
import Gsd.CommandHandler

requestCommand :: GetCommandStream -> AggregateIdStream -> GsdCommand -> IO (Either PersistenceFailure PersistResult)
requestCommand getCommandStream aggregateIdStream gsdCommand = Cqrs.persistCommands getCommandStream aggregateIdStream $ toCommand gsdCommand

runCommandConsumers :: Logger -> EventStoreContext -> IO ()
runCommandConsumers logger eventStoreContext = CommandConsumerFlow.runCommandConsumers logger (getEventStoreStreamRepository eventStoreContext) gsdCommandHandler

streamWorkspaceIds :: Streamable monad stream WorkspaceId => AggregateIdStream -> stream monad (Persisted WorkspaceId)
streamWorkspaceIds aggregateIdStream = streamAll $ aggregateIdStream


streamCommands ::  Streamable monad stream Command => GetCommandStream -> WorkspaceId -> stream monad (Persisted GsdCommand)
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