{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE GADTs #-}

module Eventuria.GSD.Write.Model.Commands.Serialization where

import Data.Aeson
import Eventuria.GSD.Write.Model.Commands.Command
import qualified Data.Text as Text


instance ToJSON GSDCommand where
  toJSON (GSDCommand gsdCommand @ CreateWorkspaceRep (CreateWorkspace {commandId, workspaceId, workspaceName})) = object [
            "commandId" .= commandId,
            "workspaceId" .= workspaceId,
            "workspaceName" .= workspaceName,
            "commandName" .= getCommandName gsdCommand]
  toJSON (GSDCommand gsdCommand  @ RenameWorkspaceRep  (RenameWorkspace {commandId, workspaceId, workspaceNewName})) = object [
              "commandId" .= commandId,
              "workspaceId" .= workspaceId,
              "workspaceNewName" .= workspaceNewName,
              "commandName" .= getCommandName gsdCommand]
  toJSON (GSDCommand gsdCommand @ SetGoalRep (SetGoal {commandId, workspaceId, goalId,goalDescription })) = object [
              "commandId" .= commandId,
              "workspaceId" .= workspaceId,
              "goalId" .= goalId,
              "goalDescription" .= goalDescription,
              "commandName" .= getCommandName gsdCommand]
  toJSON (GSDCommand gsdCommand @ RefineGoalDescriptionRep (RefineGoalDescription {commandId, workspaceId, goalId,refinedGoalDescription })) = object [
              "commandId" .= commandId,
              "workspaceId" .= workspaceId,
              "goalId" .= goalId,
              "refinedGoalDescription" .= refinedGoalDescription,
              "commandName" .= getCommandName gsdCommand]
  toJSON (GSDCommand gsdCommand @ StartWorkingOnGoalRep (StartWorkingOnGoal {commandId, workspaceId, goalId })) = object [
                "commandId" .= commandId,
                "workspaceId" .= workspaceId,
                "goalId" .= goalId,
                "commandName" .= getCommandName gsdCommand]
  toJSON (GSDCommand gsdCommand @ PauseWorkingOnGoalRep (PauseWorkingOnGoal {commandId, workspaceId, goalId } )) = object [
                "commandId" .= commandId,
                "workspaceId" .= workspaceId,
                "goalId" .= goalId,
                "commandName" .= getCommandName gsdCommand]
  toJSON (GSDCommand gsdCommand @ NotifyGoalAccomplishmentRep (NotifyGoalAccomplishment {commandId, workspaceId, goalId } )) = object [
                "commandId" .= commandId,
                "workspaceId" .= workspaceId,
                "goalId" .= goalId,
                "commandName" .= getCommandName gsdCommand]
  toJSON (GSDCommand gsdCommand @ GiveUpOnGoalRep (GiveUpOnGoal {commandId, workspaceId, goalId,reason })) = object [
                "commandId" .= commandId,
                "workspaceId" .= workspaceId,
                "goalId" .= goalId,
                "reason" .= reason,
                "commandName" .= getCommandName gsdCommand]
  toJSON (GSDCommand gsdCommand @ ActionizeOnGoalRep (ActionizeOnGoal {commandId, workspaceId, goalId,actionId,actionDetails } )) = object [
                "commandId" .= commandId,
                "workspaceId" .= workspaceId,
                "goalId" .= goalId,
                "actionId" .= actionId,
                "actionDetails" .= actionDetails,
                "commandName" .= getCommandName gsdCommand]
  toJSON (GSDCommand gsdCommand @ NotifyActionCompletedRep (NotifyActionCompleted {commandId, workspaceId, goalId,actionId } )) = object [
                "commandId" .= commandId,
                "workspaceId" .= workspaceId,
                "goalId" .= goalId,
                "actionId" .= actionId,
                "commandName" .= getCommandName gsdCommand]


instance FromJSON GSDCommand where

  parseJSON (Object jsonObject) = do
               commandNameMaybe <- jsonObject .: "commandName"
               case commandNameMaybe of
                    Just (String commandName) | (Text.unpack commandName) ==  getCommandName CreateWorkspaceRep ->
                      pure (GSDCommand CreateWorkspaceRep)
                      <*> (pure CreateWorkspace
                           <*> jsonObject .: "commandId"
                           <*> jsonObject .: "workspaceId"
                           <*> jsonObject .: "workspaceName")
                    Just (String commandName) | (Text.unpack commandName) == getCommandName RenameWorkspaceRep ->
                      pure (GSDCommand RenameWorkspaceRep)
                      <*> (pure RenameWorkspace
                           <*> jsonObject .: "commandId"
                           <*> jsonObject .: "workspaceId"
                           <*> jsonObject .: "workspaceNewName")
                    Just (String commandName) | (Text.unpack commandName) == getCommandName SetGoalRep ->
                      pure (GSDCommand SetGoalRep)
                      <*> (pure SetGoal
                          <*> jsonObject .: "commandId"
                          <*> jsonObject .: "workspaceId"
                          <*> jsonObject .: "goalId"
                          <*> jsonObject .: "goalDescription")
                    Just (String commandName) | (Text.unpack commandName) == getCommandName RefineGoalDescriptionRep ->
                        pure (GSDCommand RefineGoalDescriptionRep)
                        <*> (pure RefineGoalDescription
                            <*> jsonObject .: "commandId"
                            <*> jsonObject .: "workspaceId"
                            <*> jsonObject .: "goalId"
                            <*> jsonObject .: "refinedGoalDescription")
                    Just (String commandName) | (Text.unpack commandName) == getCommandName StartWorkingOnGoalRep ->
                        pure (GSDCommand StartWorkingOnGoalRep)
                        <*> (pure StartWorkingOnGoal
                            <*> jsonObject .: "commandId"
                            <*> jsonObject .: "workspaceId"
                            <*> jsonObject .: "goalId")
                    Just (String commandName) | (Text.unpack commandName) == getCommandName PauseWorkingOnGoalRep ->
                        pure (GSDCommand PauseWorkingOnGoalRep)
                        <*> (pure PauseWorkingOnGoal
                            <*> jsonObject .: "commandId"
                            <*> jsonObject .: "workspaceId"
                            <*> jsonObject .: "goalId")
                    Just (String commandName) | (Text.unpack commandName) == getCommandName NotifyGoalAccomplishmentRep ->
                        pure (GSDCommand NotifyGoalAccomplishmentRep)
                        <*> (pure NotifyGoalAccomplishment
                            <*> jsonObject .: "commandId"
                            <*> jsonObject .: "workspaceId"
                            <*> jsonObject .: "goalId")
                    Just (String commandName) | (Text.unpack commandName) == getCommandName GiveUpOnGoalRep ->
                        pure (GSDCommand GiveUpOnGoalRep)
                        <*> (pure GiveUpOnGoal
                            <*> jsonObject .: "commandId"
                            <*> jsonObject .: "workspaceId"
                            <*> jsonObject .: "goalId"
                            <*> jsonObject .: "reason")
                    Just (String commandName) | (Text.unpack commandName) == getCommandName ActionizeOnGoalRep ->
                        pure (GSDCommand ActionizeOnGoalRep)
                        <*> (pure ActionizeOnGoal
                            <*> jsonObject .: "commandId"
                            <*> jsonObject .: "workspaceId"
                            <*> jsonObject .: "goalId"
                            <*> jsonObject .: "actionId"
                            <*> jsonObject .: "actionDetails")
                    Just (String commandName) | (Text.unpack commandName) == getCommandName NotifyActionCompletedRep ->
                        pure (GSDCommand NotifyActionCompletedRep)
                        <*> (pure NotifyActionCompleted
                            <*> jsonObject .: "commandId"
                            <*> jsonObject .: "workspaceId"
                            <*> jsonObject .: "goalId"
                            <*> jsonObject .: "actionId")
                    Just (String unknownCommandName) -> error $ "Command unknown : " ++ Text.unpack unknownCommandName
                    _ -> error $ "Command name not provided"
  parseJSON _ =  error $ "Json format not expected"

