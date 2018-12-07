{-# LANGUAGE DuplicateRecordFields #-}
module Cqrs.Aggregate.Events.Event where

import Cqrs.Aggregate.Ids.AggregateId
import Cqrs.Aggregate.Events.EventId
import Data.Aeson
import Data.Text
import Data.Time
import Cqrs.Aggregate.Core


type Pair = (Text, Value)
type EventName = String

data Event = Event { eventHeader :: EventHeader,
                     payload :: EventPayload} deriving Show

data EventHeader =  EventHeader { aggregateId :: AggregateId,
                             eventId :: EventId ,
                             createdOn :: UTCTime ,
                             eventName :: EventName} deriving Show
type EventPayload = [Pair]



instance AggregateJoinable Event where
  getAggregateId Event { eventHeader = EventHeader {aggregateId = aggregateId} } = aggregateId





