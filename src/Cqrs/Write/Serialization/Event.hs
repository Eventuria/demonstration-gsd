{-# LANGUAGE OverloadedStrings #-}
module Cqrs.Write.Serialization.Event where

import Data.Aeson

import PersistedStreamEngine.Write.Writable
import Cqrs.Write.Aggregate.Events.Event

instance Writable Event where
  getItemName Event { eventHeader = EventHeader { eventName = eventName}} = eventName


instance ToJSON Event where
  toJSON (Event {eventHeader = eventHeader , payload = payload  } ) = object [
            "eventHeader" .= eventHeader,
            "payload" .= payload]

instance FromJSON Event where

  parseJSON (Object jsonObject) =
    Event <$> jsonObject .: "eventHeader"
            <*> jsonObject .: "payload"
  parseJSON _ =  error $ "Json format not expected"

instance ToJSON EventHeader where
  toJSON (EventHeader {aggregateId = aggregateId , eventId = eventId , createdOn = createdOn,  eventName = commandName} ) =
    object ["aggregateId" .= aggregateId,
            "eventId" .= eventId,
            "createdOn" .= createdOn,
            "eventName" .= commandName]

instance FromJSON EventHeader where

  parseJSON (Object jsonObject) =
     EventHeader <$> jsonObject .: "aggregateId"
              <*> jsonObject .: "eventId"
              <*> jsonObject .: "createdOn"
              <*> jsonObject .: "eventName"
  parseJSON _ =  error $ "Json format not expected"
