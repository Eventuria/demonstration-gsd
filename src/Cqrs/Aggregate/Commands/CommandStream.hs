{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Cqrs.Aggregate.Commands.CommandStream  where

import Cqrs.Aggregate.Commands.Command
import qualified Database.EventStore as EventStore
import qualified Data.Text as Text
import Data.UUID
import Cqrs.Aggregate.Ids.AggregateId

import Cqrs.EventStore.Stream
import Cqrs.EventStore.Context
import qualified Cqrs.EventStore.Writing as EventStore.Writing

type CommandStream = EventStoreStream Command

getCommandStream :: EventStoreContext -> AggregateId -> CommandStream
getCommandStream context aggregateId = EventStoreStream {
                                                        context = context,
                                                        streamName = (getStreamName aggregateId)}

getStreamName :: AggregateId -> EventStore.StreamName
getStreamName aggregateId = EventStore.StreamName $ Text.pack $ "aggregate_command-" ++ toString aggregateId


instance EventStore.Writing.Writable Command where
  getItemName Command { commandHeader = CommandHeader {commandName = commandName} }  = commandName