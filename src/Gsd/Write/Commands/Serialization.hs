{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Gsd.Write.Commands.Serialization where

import Data.Aeson
import Gsd.Write.Commands.Command
import qualified Data.Text as Text


instance ToJSON GsdCommand where
  toJSON (CreateWorkspace {commandId , workspaceId ,workspaceName } ) = object [
            "commandId" .= commandId,
            "workspaceId" .= workspaceId,
            "workspaceName" .= workspaceName,
            "commandName" .= createWorkspaceCommandName]
  toJSON (RenameWorkspace {commandId , workspaceId ,workspaceNewName } ) = object [
              "commandId" .= commandId,
              "workspaceId" .= workspaceId,
              "workspaceNewName" .= workspaceNewName,
              "commandName" .= renameWorkspaceCommandName]


instance FromJSON GsdCommand where

  parseJSON (Object jsonObject) = do
               commandNameMaybe <- jsonObject .: "commandName"
               case commandNameMaybe of
                    Just (String commandName) | (Text.unpack commandName) == createWorkspaceCommandName ->
                      CreateWorkspace
                          <$> jsonObject .: "commandId"
                          <*> jsonObject .: "workspaceId"
                          <*> jsonObject .: "workspaceName"
                    Just (String commandName) | (Text.unpack commandName) == renameWorkspaceCommandName ->
                      RenameWorkspace
                          <$> jsonObject .: "commandId"
                          <*> jsonObject .: "workspaceId"
                          <*> jsonObject .: "workspaceNewName"
                    Just (String unknownCommandName) -> error $ "Command unknown : " ++ Text.unpack unknownCommandName
                    _ -> error $ "Command name not provided"
  parseJSON _ =  error $ "Json format not expected"
