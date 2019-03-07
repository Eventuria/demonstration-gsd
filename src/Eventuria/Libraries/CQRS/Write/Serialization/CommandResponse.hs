{-# LANGUAGE OverloadedStrings #-}
module Eventuria.Libraries.CQRS.Write.Serialization.CommandResponse where

import Data.Aeson
import qualified Data.Text as Text
import Eventuria.Libraries.CQRS.Write.Serialization.CommandHeader()
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import Eventuria.Libraries.PersistedStreamEngine.Interface.Write.Writable

commandResponseNameForCommandSuccessfullyProcessed :: String
commandResponseNameForCommandSuccessfullyProcessed = "commandSuccessfullyProcessed"

commandResponseNameForCommandFailed :: String
commandResponseNameForCommandFailed = "commandFailed"

instance Writable CommandResponse where
  getItemName CommandSuccessfullyProcessed {} = commandResponseNameForCommandSuccessfullyProcessed
  getItemName CommandFailed {} = commandResponseNameForCommandFailed


instance ToJSON CommandResponse where
   toJSON (commandResponse @ (CommandSuccessfullyProcessed aggregateId commandId)) = object [
          "aggregateId" .= aggregateId,
          "commandId" .= commandId,
          "commandResponseName" .= commandResponseNameForCommandSuccessfullyProcessed]
   toJSON (commandResponse @ (CommandFailed aggregateId commandId reason)) = object [
         "aggregateId" .= aggregateId,
         "commandId" .= commandId,
         "commandResponseName" .= commandResponseNameForCommandFailed,
         "reason" .= reason]

instance FromJSON CommandResponse  where

  parseJSON (Object jsonObject) = do
               commandResponseNameMaybe <- jsonObject .: "commandResponseName"
               case commandResponseNameMaybe of
                    Just (String commandResponseName) | (Text.unpack commandResponseName) == commandResponseNameForCommandSuccessfullyProcessed -> CommandSuccessfullyProcessed
                        <$> jsonObject .: "aggregateId"
                        <*> jsonObject .: "commandId"
                    Just (String commandResponseName) | (Text.unpack commandResponseName) == commandResponseNameForCommandFailed -> CommandFailed
                        <$> jsonObject .: "aggregateId"
                        <*> jsonObject .: "commandId"
                        <*> jsonObject .: "reason"
                    Just (String unknownCommandResponseName) -> error $ "Command Response unknown : " ++ Text.unpack unknownCommandResponseName
                    Nothing -> error $ "Command Response name not provided"
                    _ -> error $ "Json format not expected"
  parseJSON _ = error $ "Json format not expected"