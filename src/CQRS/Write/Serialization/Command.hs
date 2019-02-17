{-# LANGUAGE OverloadedStrings #-}
module CQRS.Write.Serialization.Command where

import Data.Aeson

import CQRS.Write.Aggregate.Commands.Command
import PersistedStreamEngine.Interface.Write.Writable
import CQRS.Write.Serialization.CommandHeader()
import CQRS.Write.Aggregate.Commands.CommandHeader

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



