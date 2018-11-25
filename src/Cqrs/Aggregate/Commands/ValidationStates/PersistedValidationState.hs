{-# LANGUAGE OverloadedStrings         #-}
module Cqrs.Aggregate.Commands.ValidationStates.PersistedValidationState where

import Cqrs.Aggregate.Commands.ValidationStates.ValidationState
import Cqrs.Streams
import Data.Aeson

data PersistedValidationState = PersistedValidationState { offset :: Offset, validationState :: ValidationState } deriving (Show,Eq)

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