{-# LANGUAGE OverloadedStrings #-}
module Cqrs.Write.Serialization.CommandResponse where

import Data.Aeson
import qualified Data.Text as Text

import Cqrs.Write.Aggregate.Commands.Responses.CommandResponse
import PersistedStreamEngine.Interface.Write.Writable

commandResponseNameForCommandSuccessfullyProcessed :: String
commandResponseNameForCommandSuccessfullyProcessed = "commandSuccessfullyProcessed"

commandResponseNameForCommandSkippedBecauseAlreadyProcessed :: String
commandResponseNameForCommandSkippedBecauseAlreadyProcessed = "commandSkippedBecauseAlreadyProcessed"

commandResponseNameForCommandFailed :: String
commandResponseNameForCommandFailed = "commandFailed"

instance Writable CommandResponse where
  getItemName CommandSuccessfullyProcessed {} = commandResponseNameForCommandSuccessfullyProcessed
  getItemName CommandSkippedBecauseAlreadyProcessed {} = commandResponseNameForCommandSkippedBecauseAlreadyProcessed
  getItemName CommandFailed {} = commandResponseNameForCommandFailed


instance ToJSON CommandResponse where
   toJSON (commandResponse @ (CommandSuccessfullyProcessed commandId aggregateId)) = object [
          "commandId" .= commandId,
          "aggregateId" .= aggregateId,
          "commandResponseName" .= commandResponseNameForCommandSuccessfullyProcessed]
   toJSON (commandResponse @ (CommandSkippedBecauseAlreadyProcessed commandId aggregateId)) = object [
             "commandId" .= commandId,
             "aggregateId" .= aggregateId,
             "commandResponseName" .= commandResponseNameForCommandSkippedBecauseAlreadyProcessed]
   toJSON (commandResponse @ (CommandFailed commandId aggregateId reason)) = object [
             "commandId" .= commandId,
             "aggregateId" .= aggregateId,
             "commandResponseName" .= commandResponseNameForCommandFailed,
             "reason" .= reason]

instance FromJSON CommandResponse  where

  parseJSON (Object jsonObject) = do
               commandResponseNameMaybe <- jsonObject .: "commandResponseName"
               case commandResponseNameMaybe of
                    Just (String commandResponseName) | (Text.unpack commandResponseName) == commandResponseNameForCommandSuccessfullyProcessed -> CommandSuccessfullyProcessed
                        <$> jsonObject .: "commandId"
                        <*> jsonObject .: "aggregateId"
                    Just (String commandResponseName) | (Text.unpack commandResponseName) == commandResponseNameForCommandSkippedBecauseAlreadyProcessed -> CommandSkippedBecauseAlreadyProcessed
                                            <$> jsonObject .: "commandId"
                                            <*> jsonObject .: "aggregateId"
                    Just (String commandResponseName) | (Text.unpack commandResponseName) == commandResponseNameForCommandFailed -> CommandFailed
                        <$> jsonObject .: "commandId"
                        <*> jsonObject .: "aggregateId"
                        <*> jsonObject .: "ideaContent"
                    Just (String unknownCommandResponseName) -> error $ "Command Response unknown : " ++ Text.unpack unknownCommandResponseName
                    Nothing -> error $ "Command Response name not provided"
                    _ -> error $ "Json format not expected"
  parseJSON _ = error $ "Json format not expected"