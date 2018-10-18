{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module CommandSourcing.Commands where

import Data.UUID
import Data.Time
import Data.Aeson
import CommandSourcing.Core
import qualified Data.Text as Text

type CommandName = String
data Command  = CreateWorkspace { commandId :: CommandId , workspaceId ::WorkspaceId }
                     | IntroduceIdea { commandId :: CommandId , workspaceId ::WorkspaceId , ideaContent :: String}
                     | NonDeserializedCommand deriving (Show,Eq)

serializedCommandNameForCreateWorkspace :: CommandName
serializedCommandNameForCreateWorkspace = "createWorkspace"

serializedCommandNameForIntroduceIdea :: CommandName
serializedCommandNameForIntroduceIdea = "introduceIdea"

class WorkspaceIdRetrievable command where
  getWorkspaceId :: command -> WorkspaceId

instance WorkspaceIdRetrievable Command where
  getWorkspaceId CreateWorkspace { workspaceId = workspaceId} = workspaceId
  getWorkspaceId IntroduceIdea   { workspaceId = workspaceId} = workspaceId

class CommandIdRetrievable command where
  getCommandId :: command -> CommandId

instance CommandIdRetrievable Command where
  getCommandId CreateWorkspace { commandId = commandId} = commandId
  getCommandId IntroduceIdea   { commandId = commandId} = commandId

class CommandSerializable command where
  getCommandName :: command -> CommandName

instance CommandSerializable Command where
  getCommandName CreateWorkspace {} = serializedCommandNameForCreateWorkspace
  getCommandName IntroduceIdea {} = serializedCommandNameForIntroduceIdea

instance ToJSON Command where
   toJSON (command @ (CreateWorkspace commandId workspaceId)) = object [
          "commandId" .= commandId,
          "workspaceId" .= workspaceId,
          "commandName" .= getCommandName command]
   toJSON (command @ (IntroduceIdea commandId workspaceId ideaContent)) = object [
             "commandId" .= commandId,
             "workspaceId" .= workspaceId,
             "commandName" .= getCommandName command,
             "ideaContent" .= ideaContent]

instance FromJSON Command  where

    parseJSON (Object jsonObject) = do
             commandNameMaybe <- jsonObject .: "commandName"
             case commandNameMaybe of
                  Just (String commandName) | (Text.unpack commandName) == serializedCommandNameForCreateWorkspace -> CreateWorkspace
                      <$> jsonObject .: "commandId"
                      <*> jsonObject .: "workspaceId"
                  Just (String commandName) | (Text.unpack commandName) == serializedCommandNameForIntroduceIdea -> IntroduceIdea
                      <$> jsonObject .: "commandId"
                      <*> jsonObject .: "workspaceId"
                      <*> jsonObject .: "ideaContent"
                  Just (String unknownCommandName) -> error $ "Command unknown : " ++ Text.unpack unknownCommandName
                  Nothing -> error $ "Command name not provided"

