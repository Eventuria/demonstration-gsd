{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Cqrs.CommandResponse where

import Data.UUID
import Data.Time
import Data.Aeson
import qualified Data.Text as Text

data CommandResponse  = CommandSuccessfullyProcessed { commandId :: UUID , workspaceId ::UUID }
                      | CommandFailed { commandId :: UUID , workspaceId ::UUID , reason :: String}  deriving (Show,Eq)

commandResponseNameForCommandSuccessfullyProcessed :: String
commandResponseNameForCommandSuccessfullyProcessed = "commandSuccessfullyProcessed"

commandResponseNameForCommandFailed :: String
commandResponseNameForCommandFailed = "commandFailed"

class CommandResponseSerializable command where
  getCommandResponseName :: command -> String
  getCommandId :: command -> UUID


instance CommandResponseSerializable CommandResponse where
  getCommandResponseName CommandSuccessfullyProcessed {} = commandResponseNameForCommandSuccessfullyProcessed
  getCommandResponseName CommandFailed {} = commandResponseNameForCommandFailed

  getCommandId CommandSuccessfullyProcessed { commandId = commandId} = commandId
  getCommandId CommandFailed { commandId = commandId} = commandId

instance ToJSON CommandResponse where
   toJSON (commandResponse @ (CommandSuccessfullyProcessed commandId workspaceId)) = object [
          "commandId" .= commandId,
          "workspaceId" .= workspaceId,
          "commandResponseName" .= getCommandResponseName commandResponse]
   toJSON (commandResponse @ (CommandFailed commandId workspaceId reason)) = object [
             "commandId" .= commandId,
             "workspaceId" .= workspaceId,
             "commandResponseName" .= getCommandResponseName commandResponse,
             "reason" .= reason]

instance FromJSON CommandResponse  where

  parseJSON (Object jsonObject) = do
               commandResponseNameMaybe <- jsonObject .: "commandResponseName"
               case commandResponseNameMaybe of
                    Just (String commandResponseName) | (Text.unpack commandResponseName) == commandResponseNameForCommandSuccessfullyProcessed -> CommandSuccessfullyProcessed
                        <$> jsonObject .: "commandId"
                        <*> jsonObject .: "workspaceId"
                    Just (String commandResponseName) | (Text.unpack commandResponseName) == commandResponseNameForCommandFailed -> CommandFailed
                        <$> jsonObject .: "commandId"
                        <*> jsonObject .: "workspaceId"
                        <*> jsonObject .: "ideaContent"
                    Just (String unknownCommandResponseName) -> error $ "Command Response unknown : " ++ Text.unpack unknownCommandResponseName
                    Nothing -> error $ "Command Response name not provided"
