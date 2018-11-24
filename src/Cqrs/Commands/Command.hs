{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Cqrs.Commands.Command where

import Cqrs.Aggregate.Ids.AggregateId
import Cqrs.Commands.CommandId
import Data.Aeson
import Data.Text
import Cqrs.Core

type Pair = (Text, Value)

data Command = Command { commandHeader :: CommandHeader,
                         payload :: CommandPayload}

data CommandHeader =  CommandHeader { aggregateId :: AggregateId,
                               commandId :: CommandId ,
                               commandName :: CommandName}
type CommandPayload = [Pair]


instance ToJSON Command where
  toJSON (Command {commandHeader = commandHeader , payload = payload  } ) = object [
            "commandHeader" .= commandHeader,
            "payload" .= payload]

instance FromJSON Command where

  parseJSON (Object jsonObject) =
    Command <$> jsonObject .: "commandHeader"
            <*> jsonObject .: "payload"
  parseJSON _ =  error $ "Json format not expected"


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

