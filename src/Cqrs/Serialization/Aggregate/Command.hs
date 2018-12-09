{-# LANGUAGE OverloadedStrings #-}
module Cqrs.Serialization.Aggregate.Command where

import Data.Aeson

import Cqrs.Aggregate.Commands.Command
import Cqrs.PersistedStreamEngine.Write.Writable

instance Writable Command where
  getItemName Command { commandHeader = CommandHeader {commandName = commandName} }  = commandName


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
