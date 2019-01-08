{-# LANGUAGE OverloadedStrings #-}
module Cqrs.Write.Serialization.Command where

import Data.Aeson

import Cqrs.Write.Aggregate.Commands.Command
import PersistedStreamEngine.Interface.Write.Writable
import Cqrs.Write.Serialization.CommandHeader()
import Cqrs.Write.Aggregate.Commands.CommandHeader

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



