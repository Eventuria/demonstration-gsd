{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Cqrs.CommandResponse where

import Data.UUID
import Data.Time
import Data.Aeson
import Cqrs.Core
import qualified Data.Text as Text

type RejectionReason = String

data CommandResponse  = CommandSuccessfullyProcessed { commandId :: CommandId , aggregateId :: AggregateId }
                      | CommandSkippedBecauseAlreadyProcessed { commandId :: CommandId , aggregateId ::AggregateId }
                      | CommandFailed { commandId :: CommandId , aggregateId ::AggregateId , reason :: RejectionReason}  deriving (Show,Eq)

commandResponseNameForCommandSuccessfullyProcessed :: String
commandResponseNameForCommandSuccessfullyProcessed = "commandSuccessfullyProcessed"

commandResponseNameForCommandSkippedBecauseAlreadyProcessed :: String
commandResponseNameForCommandSkippedBecauseAlreadyProcessed = "commandSkippedBecauseAlreadyProcessed"

commandResponseNameForCommandFailed :: String
commandResponseNameForCommandFailed = "commandFailed"

class CommandResponseSerializable command where
  getCommandResponseName :: command -> String
  getCommandId :: command -> CommandId


instance CommandResponseSerializable CommandResponse where
  getCommandResponseName CommandSuccessfullyProcessed {} = commandResponseNameForCommandSuccessfullyProcessed
  getCommandResponseName CommandSkippedBecauseAlreadyProcessed {} = commandResponseNameForCommandSkippedBecauseAlreadyProcessed
  getCommandResponseName CommandFailed {} = commandResponseNameForCommandFailed

  getCommandId CommandSuccessfullyProcessed { commandId = commandId} = commandId
  getCommandId CommandSkippedBecauseAlreadyProcessed { commandId = commandId} = commandId
  getCommandId CommandFailed { commandId = commandId} = commandId

instance ToJSON CommandResponse where
   toJSON (commandResponse @ (CommandSuccessfullyProcessed commandId aggregateId)) = object [
          "commandId" .= commandId,
          "aggregateId" .= aggregateId,
          "commandResponseName" .= getCommandResponseName commandResponse]
   toJSON (commandResponse @ (CommandSkippedBecauseAlreadyProcessed commandId aggregateId)) = object [
             "commandId" .= commandId,
             "aggregateId" .= aggregateId,
             "commandResponseName" .= getCommandResponseName commandResponse]
   toJSON (commandResponse @ (CommandFailed commandId aggregateId reason)) = object [
             "commandId" .= commandId,
             "aggregateId" .= aggregateId,
             "commandResponseName" .= getCommandResponseName commandResponse,
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
