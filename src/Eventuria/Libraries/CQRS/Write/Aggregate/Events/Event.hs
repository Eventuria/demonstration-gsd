{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module Eventuria.Libraries.CQRS.Write.Aggregate.Events.Event where

import Data.Aeson
import Data.Time
import Data.Map

import GHC.Generics

import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId
import Eventuria.Libraries.CQRS.Write.Aggregate.Events.EventId
import Eventuria.Libraries.CQRS.Write.Aggregate.Core

type EventName = String

data Event = Event { eventHeader :: EventHeader,
                     payload :: EventPayload} deriving (Show,Eq,Generic)

data EventHeader =  EventHeader { aggregateId :: AggregateId,
                             eventId :: EventId ,
                             createdOn :: UTCTime ,
                             eventName :: EventName} deriving (Show,Eq,Generic)
type EventPayload = Map String Value

instance AggregateJoinable Event where
  getAggregateId Event { eventHeader = EventHeader {aggregateId = aggregateId} } = aggregateId





