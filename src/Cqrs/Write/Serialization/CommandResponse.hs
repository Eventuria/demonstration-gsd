{-# LANGUAGE OverloadedStrings #-}
module Cqrs.Write.Serialization.CommandResponse where

import Data.Aeson
import qualified Data.Text as Text
import Cqrs.Write.Serialization.CommandHeader()
import Cqrs.Write.Aggregate.Commands.Responses.CommandResponse
import PersistedStreamEngine.Interface.Write.Writable

commandResponseNameForCommandSuccessfullyProcessed :: String
commandResponseNameForCommandSuccessfullyProcessed = "commandSuccessfullyProcessed"

commandResponseNameForCommandFailed :: String
commandResponseNameForCommandFailed = "commandFailed"

instance Writable CommandResponse where
  getItemName CommandSuccessfullyProcessed {} = commandResponseNameForCommandSuccessfullyProcessed
  getItemName CommandFailed {} = commandResponseNameForCommandFailed


instance ToJSON CommandResponse where
   toJSON (commandResponse @ (CommandSuccessfullyProcessed commandHeaderProcessed)) = object [
          "commandHeaderProcessed" .= commandHeaderProcessed,
          "commandResponseName" .= commandResponseNameForCommandSuccessfullyProcessed]
   toJSON (commandResponse @ (CommandFailed commandHeaderProcessed reason)) = object [
         "commandHeaderProcessed" .= commandHeaderProcessed,
         "commandResponseName" .= commandResponseNameForCommandFailed,
         "reason" .= reason]

instance FromJSON CommandResponse  where

  parseJSON (Object jsonObject) = do
               commandResponseNameMaybe <- jsonObject .: "commandResponseName"
               case commandResponseNameMaybe of
                    Just (String commandResponseName) | (Text.unpack commandResponseName) == commandResponseNameForCommandSuccessfullyProcessed -> CommandSuccessfullyProcessed
                        <$> jsonObject .: "commandHeaderProcessed"
                    Just (String commandResponseName) | (Text.unpack commandResponseName) == commandResponseNameForCommandFailed -> CommandFailed
                        <$> jsonObject .: "commandHeaderProcessed"
                        <*> jsonObject .: "reason"
                    Just (String unknownCommandResponseName) -> error $ "Command Response unknown : " ++ Text.unpack unknownCommandResponseName
                    Nothing -> error $ "Command Response name not provided"
                    _ -> error $ "Json format not expected"
  parseJSON _ = error $ "Json format not expected"