{-# LANGUAGE OverloadedStrings #-}
module Eventuria.Libraries.CQRS.Write.Serialization.Command where

import Data.Aeson

import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import Eventuria.Libraries.PersistedStreamEngine.Interface.Write.Writable
import Eventuria.Libraries.CQRS.Write.Serialization.CommandHeader()
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandHeader

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



