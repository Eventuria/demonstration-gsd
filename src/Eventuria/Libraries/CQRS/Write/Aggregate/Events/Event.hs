{-# LANGUAGE DuplicateRecordFields #-}
module Eventuria.Libraries.CQRS.Write.Aggregate.Events.Event where

import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId
import Eventuria.Libraries.CQRS.Write.Aggregate.Events.EventId
import Data.Aeson
import Data.Time
import Eventuria.Libraries.CQRS.Write.Aggregate.Core
import Data.Map


type EventName = String

data Event = Event { eventHeader :: EventHeader,
                     payload :: EventPayload} deriving (Show,Eq)

data EventHeader =  EventHeader { aggregateId :: AggregateId,
                             eventId :: EventId ,
                             createdOn :: UTCTime ,
                             eventName :: EventName} deriving (Show,Eq)
type EventPayload = Map String Value

instance AggregateJoinable Event where
  getAggregateId Event { eventHeader = EventHeader {aggregateId = aggregateId} } = aggregateId





