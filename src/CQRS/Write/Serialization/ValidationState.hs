{-# LANGUAGE OverloadedStrings #-}
module CQRS.Write.Serialization.ValidationState where

import Data.Aeson

import PersistedStreamEngine.Interface.Write.Writable
import CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState

instance ToJSON state => Writable (ValidationState state) where
  getItemName validationState  = "validationState"


instance ToJSON state => ToJSON (ValidationState state) where
   toJSON (ValidationState lastOffsetConsumed commandsProcessed aggregateId state) = object [
      "lastOffsetConsumed" .= lastOffsetConsumed,
      "commandsProcessed" .= commandsProcessed,
      "aggregateId" .= aggregateId,
      "state" .= state
      ]

instance FromJSON state => FromJSON (ValidationState state) where

    parseJSON (Object jsonObject) = ValidationState <$> jsonObject .: "lastOffsetConsumed"
                                             <*> jsonObject .: "commandsProcessed"
                                             <*> jsonObject .: "aggregateId"
                                             <*> jsonObject .: "state"
    parseJSON _ =  error $ "Json format not expected"

