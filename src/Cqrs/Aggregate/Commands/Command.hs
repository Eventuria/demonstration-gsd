{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Cqrs.Aggregate.Commands.Command where

import Cqrs.Aggregate.Ids.AggregateId
import Cqrs.Aggregate.Commands.CommandId
import Data.Aeson
import Data.Text
import Cqrs.Aggregate.Core

type Pair = (Text, Value)
type CommandName = String

data Command = Command { commandHeader :: CommandHeader,
                         payload :: CommandPayload}

data CommandHeader =  CommandHeader { aggregateId :: AggregateId,
                               commandId :: CommandId ,
                               commandName :: CommandName}
type CommandPayload = [Pair]

class CommandJoinable a where
 getCommandId :: a -> CommandId

instance AggregateJoinable Command where
  getAggregateId Command { commandHeader = CommandHeader {aggregateId = aggregateId} } = aggregateId


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

