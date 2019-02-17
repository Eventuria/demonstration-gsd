{-# LANGUAGE OverloadedStrings #-}
module CQRS.Write.Serialization.CommandHeader where

import Data.Aeson
import CQRS.Write.Aggregate.Commands.CommandHeader

instance ToJSON CommandHeader where
  toJSON (CommandHeader {aggregateId = aggregateId , commandId = commandId ,  commandName = commandName} ) =
    object ["aggregateId" .= aggregateId,
            "commandId" .= commandId,
            "commandName" .= commandName]

instance FromJSON CommandHeader where

  parseJSON (Object jsonObject) =
     CommandHeader <$> jsonObject .: "aggregateId"
              <*> jsonObject .: "commandId"
              <*> jsonObject .: "commandName"
  parseJSON _ =  error $ "Json format not expected"
