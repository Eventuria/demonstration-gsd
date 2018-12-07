{-# LANGUAGE OverloadedStrings #-}
module Cqrs.Serialization.Aggregate.ValidationState where

import Data.Aeson

import Cqrs.PersistedStream.Write.Writable
import Cqrs.Aggregate.Commands.ValidationStates.ValidationState

instance Writable ValidationState where
  getItemName validationState  = "validationState"


instance ToJSON AggregateState where
   toJSON (AggregateState aggregateId) = object ["aggregateId" .= aggregateId]

instance ToJSON ValidationState where
   toJSON (ValidationState lastOffsetConsumed commandsProcessed state) = object [
      "lastOffsetConsumed" .= lastOffsetConsumed,
      "commandsProcessed" .= commandsProcessed,
      "state" .= state
      ]

instance FromJSON ValidationState  where

    parseJSON (Object jsonObject) = ValidationState <$> jsonObject .: "lastOffsetConsumed"
                                             <*> jsonObject .: "commandsProcessed"
                                             <*> jsonObject .: "state"
    parseJSON _ =  error $ "Json format not expected"

instance FromJSON AggregateState  where

    parseJSON (Object jsonObject) = AggregateState <$> jsonObject .: "aggregateId"
    parseJSON _ =  error $ "Json format not expected"