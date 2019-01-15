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
  toJSON (SetGoal {commandId , workspaceId ,goalId , goalDescription } ) = object [
              "commandId" .= commandId,
              "workspaceId" .= workspaceId,
              "goalId" .= goalId,
              "goalDescription" .= goalDescription,
              "commandName" .= setGoalCommandName]
  toJSON (RefineGoalDescription {commandId , workspaceId ,goalId , refinedGoalDescription } ) = object [
              "commandId" .= commandId,
              "workspaceId" .= workspaceId,
              "goalId" .= goalId,
              "refinedGoalDescription" .= refinedGoalDescription,
              "commandName" .= refineGoalDescriptionCommandName]
  toJSON (StartWorkingOnGoal {commandId , workspaceId ,goalId  } ) = object [
                "commandId" .= commandId,
                "workspaceId" .= workspaceId,
                "goalId" .= goalId,
                "commandName" .= startWorkingOnGoalCommandName]
  toJSON (PauseWorkingOnGoal {commandId , workspaceId ,goalId  } ) = object [
                "commandId" .= commandId,
                "workspaceId" .= workspaceId,
                "goalId" .= goalId,
                "commandName" .= pauseWorkingOnGoalCommandName]
  toJSON (NotifyGoalAccomplishment {commandId , workspaceId ,goalId  } ) = object [
                "commandId" .= commandId,
                "workspaceId" .= workspaceId,
                "goalId" .= goalId,
                "commandName" .= notifyGoalAccomplishmentCommandName]
  toJSON (GiveUpOnGoal {commandId , workspaceId ,goalId,reason  } ) = object [
                "commandId" .= commandId,
                "workspaceId" .= workspaceId,
                "goalId" .= goalId,
                "reason" .= reason,
                "commandName" .= giveUpOnGoalCommandName]


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
                    Just (String commandName) | (Text.unpack commandName) == setGoalCommandName ->
                      SetGoal
                          <$> jsonObject .: "commandId"
                          <*> jsonObject .: "workspaceId"
                          <*> jsonObject .: "goalId"
                          <*> jsonObject .: "goalDescription"
                    Just (String commandName) | (Text.unpack commandName) == refineGoalDescriptionCommandName ->
                        RefineGoalDescription
                            <$> jsonObject .: "commandId"
                            <*> jsonObject .: "workspaceId"
                            <*> jsonObject .: "goalId"
                            <*> jsonObject .: "refinedGoalDescription"
                    Just (String commandName) | (Text.unpack commandName) == startWorkingOnGoalCommandName ->
                        StartWorkingOnGoal
                            <$> jsonObject .: "commandId"
                            <*> jsonObject .: "workspaceId"
                            <*> jsonObject .: "goalId"
                    Just (String commandName) | (Text.unpack commandName) == pauseWorkingOnGoalCommandName ->
                        PauseWorkingOnGoal
                            <$> jsonObject .: "commandId"
                            <*> jsonObject .: "workspaceId"
                            <*> jsonObject .: "goalId"
                    Just (String commandName) | (Text.unpack commandName) == notifyGoalAccomplishmentCommandName ->
                        NotifyGoalAccomplishment
                            <$> jsonObject .: "commandId"
                            <*> jsonObject .: "workspaceId"
                            <*> jsonObject .: "goalId"
                    Just (String commandName) | (Text.unpack commandName) == giveUpOnGoalCommandName ->
                        GiveUpOnGoal
                            <$> jsonObject .: "commandId"
                            <*> jsonObject .: "workspaceId"
                            <*> jsonObject .: "goalId"
                            <*> jsonObject .: "reason"
                    Just (String unknownCommandName) -> error $ "Command unknown : " ++ Text.unpack unknownCommandName
                    _ -> error $ "Command name not provided"
  parseJSON _ =  error $ "Json format not expected"
