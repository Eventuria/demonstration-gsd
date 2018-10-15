{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module CommandSourcing.CommandResponse where

import Data.UUID
import Data.Time
import Data.Aeson
import qualified Data.Text as Text

data CommandResponse  = CommandSuccessfullyProcessed { commandId :: UUID , workspaceId ::UUID }
                      | CommandFailed { commandId :: UUID , workspaceId ::UUID , reason :: String}  deriving (Show,Eq)

serializedCommandResponseNameForCommandSuccessfullyProcessed :: String
serializedCommandResponseNameForCommandSuccessfullyProcessed = "commandSuccessfullyProcessed"

serializedCommandResponseNameForCommandFailed :: String
serializedCommandResponseNameForCommandFailed = "commandFailed"

class CommandResponseSerializable command where
  serializedCommandResponseName :: command -> String


instance CommandResponseSerializable CommandResponse where
  serializedCommandResponseName CommandSuccessfullyProcessed {} = serializedCommandResponseNameForCommandSuccessfullyProcessed
  serializedCommandResponseName CommandFailed {} = serializedCommandResponseNameForCommandFailed

instance ToJSON CommandResponse where
   toJSON (commandResponse @ (CommandSuccessfullyProcessed commandId workspaceId)) = object [
          "commandId" .= commandId,
          "workspaceId" .= workspaceId,
          "commandResponseName" .= serializedCommandResponseName commandResponse]
   toJSON (commandResponse @ (CommandFailed commandId workspaceId reason)) = object [
             "commandId" .= commandId,
             "workspaceId" .= workspaceId,
             "commandResponseName" .= serializedCommandResponseName commandResponse,
             "reason" .= reason]

instance FromJSON CommandResponse  where

  parseJSON (Object jsonObject) = do
               commandResponseNameMaybe <- jsonObject .: "commandResponseName"
               case commandResponseNameMaybe of
                    Just (String commandResponseName) | (Text.unpack commandResponseName) == serializedCommandResponseNameForCommandSuccessfullyProcessed -> CommandSuccessfullyProcessed
                        <$> jsonObject .: "commandId"
                        <*> jsonObject .: "workspaceId"
                    Just (String commandResponseName) | (Text.unpack commandResponseName) == serializedCommandResponseNameForCommandFailed -> CommandFailed
                        <$> jsonObject .: "commandId"
                        <*> jsonObject .: "workspaceId"
                        <*> jsonObject .: "ideaContent"
                    Just (String unknownCommandResponseName) -> error $ "Command Response unknown : " ++ Text.unpack unknownCommandResponseName
                    Nothing -> error $ "Command Response name not provided"
