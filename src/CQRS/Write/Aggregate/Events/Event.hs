{-# LANGUAGE DuplicateRecordFields #-}
module CQRS.Write.Aggregate.Events.Event where

import CQRS.Write.Aggregate.Ids.AggregateId
import CQRS.Write.Aggregate.Events.EventId
import Data.Aeson
import Data.Time
import CQRS.Write.Aggregate.Core
import Data.Map


type EventName = String

data Event = Event { eventHeader :: EventHeader,
                     payload :: EventPayload} deriving Show

data EventHeader =  EventHeader { aggregateId :: AggregateId,
                             eventId :: EventId ,
                             createdOn :: UTCTime ,
                             eventName :: EventName} deriving Show
type EventPayload = Map String Value

instance AggregateJoinable Event where
  getAggregateId Event { eventHeader = EventHeader {aggregateId = aggregateId} } = aggregateId





