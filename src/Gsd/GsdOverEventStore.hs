{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Gsd.GsdOverEventStore (requestCommand,
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
import Cqrs.Aggregate.Ids.AggregateId
import Cqrs.PersistedStream.PersistedItem
import Cqrs.Aggregate.Commands.Command

import Gsd.CommandHandler
import Gsd.Commands
import Gsd.Core

import Plugins.GregYoungEventStore.Read.Streaming
import Plugins.GregYoungEventStore.Settings
import Plugins.GregYoungEventStore.Stream
import Plugins.GregYoungEventStore.InterpreterEventStore
import Plugins.GregYoungEventStore.Instance

import Cqrs.PersistedStream.Write.PersistenceResult

requestCommand ::  EventStoreSettings -> GsdCommand -> IO (Either PersistenceFailure PersistResult)
requestCommand settings gsdCommand =
  Cqrs.persistCommands
    getEventStoreWriting
    getEventStoreQuerying
    (getCommandStream  $ getEventStoreStreamRepository settings)
    (aggregateIdStream $ getEventStoreStreamRepository settings) $ toCommand gsdCommand

runCommandConsumers :: EventStoreSettings -> Logger ->  IO ()
runCommandConsumers settings logger  =
   CommandConsumerFlow.runCommandConsumers
      logger
      (getEventStoreStreamRepository settings)
      getEventStoreReading
      gsdCommandHandler
      interpretWriteEventStoreLanguage

streamWorkspaceIds :: Streamable stream monad WorkspaceId => EventStoreSettings -> stream monad (Persisted WorkspaceId)
streamWorkspaceIds settings = streamAll (aggregateIdStream $ getEventStoreStreamRepository settings)


streamCommands ::  Streamable stream monad Command => EventStoreSettings -> WorkspaceId -> stream monad (Persisted GsdCommand)
streamCommands settings workspaceId = do
  (streamAll $ (getCommandStream  $ getEventStoreStreamRepository settings) workspaceId) &
      S.map (\PersistedItem { offset = offset, item = cqrsCommand} ->
              PersistedItem { offset = offset, item = fromJust $ fromCommand $ cqrsCommand})


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