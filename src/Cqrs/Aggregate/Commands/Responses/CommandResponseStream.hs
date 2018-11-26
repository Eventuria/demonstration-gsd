{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Cqrs.Aggregate.Commands.Responses.CommandResponseStream where

import qualified Database.EventStore as EventStore
import Cqrs.Aggregate.Commands.Responses.CommandResponse
import Cqrs.EventStore.Stream
import qualified Data.Text as Text
import Data.UUID
import Cqrs.Aggregate.Ids.AggregateId
import Cqrs.EventStore.Context


type CommandResponseStream = EventStoreStream CommandResponse

getResponseStream :: EventStoreContext -> AggregateId -> CommandResponseStream
getResponseStream context aggregateId = EventStoreStream {
                                                        context = context,
                                                        streamName = (getStreamName aggregateId)}

getStreamName :: AggregateId -> EventStore.StreamName
getStreamName aggregateId = EventStore.StreamName $ Text.pack $ "aggregate_response_command-" ++ (toString $ aggregateId)

