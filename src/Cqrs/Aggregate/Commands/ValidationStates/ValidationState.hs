{-# LANGUAGE OverloadedStrings #-}
module Cqrs.Aggregate.Commands.ValidationStates.ValidationState where

import Cqrs.Streams
import Data.Set (Set)
import Cqrs.Aggregate.Commands.CommandId
import Cqrs.Aggregate.Ids.AggregateId
import Cqrs.Aggregate.Core
import Data.Aeson
import qualified Plugins.GregYoungEventStore.Write.Persisting as EventStore.Writing



data ValidationState = ValidationState { lastOffsetConsumed::Offset , commandsProcessed :: Set CommandId, state :: AggregateState } deriving (Eq)

instance AggregateJoinable ValidationState where
  getAggregateId ValidationState { state = AggregateState {aggregateId = aggregateId} } = aggregateId



instance Show ValidationState where
  show validationState = "ValidationState { offset = " ++ ( show $ lastOffsetConsumed validationState)  ++ " }"

data AggregateState = AggregateState {aggregateId :: AggregateId } deriving (Eq,Show)

instance EventStore.Writing.Writable ValidationState where
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