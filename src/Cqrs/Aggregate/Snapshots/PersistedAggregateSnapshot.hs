{-# LANGUAGE OverloadedStrings         #-}
module Cqrs.Aggregate.Snapshots.PersistedAggregateSnapshot where

import Cqrs.Aggregate.Snapshots.AggregateSnapshot
import Cqrs.Streams
import Data.Aeson

data PersistedAggregateSnapshot = PersistedAggregateSnapshot { offset :: Offset, aggregateSnapshot :: AggregateSnapshot } deriving (Show,Eq)

instance ToJSON AggregateState where
   toJSON (AggregateState aggregateId) = object ["aggregateId" .= aggregateId]

instance ToJSON AggregateSnapshot where
   toJSON (AggregateSnapshot lastOffsetConsumed commandsProcessed state) = object [
      "lastOffsetConsumed" .= lastOffsetConsumed,
      "commandsProcessed" .= commandsProcessed,
      "state" .= state
      ]

instance FromJSON AggregateSnapshot  where

    parseJSON (Object jsonObject) = AggregateSnapshot <$> jsonObject .: "lastOffsetConsumed"
                                             <*> jsonObject .: "commandsProcessed"
                                             <*> jsonObject .: "state"
    parseJSON _ =  error $ "Json format not expected"

instance FromJSON AggregateState  where

    parseJSON (Object jsonObject) = AggregateState <$> jsonObject .: "aggregateId"
    parseJSON _ =  error $ "Json format not expected"