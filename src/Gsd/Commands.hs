{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Gsd.Commands where

import Data.Aeson

import Gsd.Core

import Cqrs.Write.Aggregate.Commands.CommandId
import Cqrs.Write.Aggregate.Commands.Command
import qualified Cqrs.Write.Aggregate.Commands.Command as CommandModule

import qualified Data.Text as Text

data GsdCommand =  CreateWorkspace { commandId :: CommandId , workspaceId ::WorkspaceId }
                 | SetGoal  { commandId :: CommandId ,
                              workspaceId ::WorkspaceId ,
                              goalId :: GoalId ,
                              goalDetails :: String}
                 | QuestionGoal {  commandId :: CommandId ,
                                   workspaceId ::WorkspaceId ,
                                   goalId :: GoalId ,
                                   questionId :: QuestionId ,
                                   questionDetails :: String}
                 | ActionizeOnQuestion {
                                       commandId :: CommandId ,
                                       workspaceId ::WorkspaceId ,
                                       questionId :: QuestionId ,
                                       actionId :: ActionId ,
                                       actionDetails :: String}
                 | ActionizeOnTheGoalDirectly {
                                       commandId :: CommandId ,
                                       workspaceId ::WorkspaceId ,
                                       goalId :: GoalId ,
                                       actionId :: ActionId ,
                                       actionDetails :: String}

createWorkspaceCommandName :: String
createWorkspaceCommandName = "createWorkspace"

isCreateWorkspaceCommand :: Command -> Bool
isCreateWorkspaceCommand command = (commandName $ commandHeader command) == createWorkspaceCommandName

toCommand :: GsdCommand -> Command
toCommand  CreateWorkspace {commandId = commandId, workspaceId = workspaceId} =
  Command { commandHeader = CommandHeader { commandId = commandId, aggregateId = workspaceId , commandName = createWorkspaceCommandName } ,
            payload = []}
toCommand _ = error "to handle..."

fromCommand :: Command -> Maybe GsdCommand
fromCommand command =
  case (commandName $ commandHeader command) of
    "createWorkspace" -> Just CreateWorkspace {commandId = CommandModule.commandId $ commandHeader command, workspaceId = aggregateId $ commandHeader command}
    _ -> Nothing


instance ToJSON GsdCommand where
  toJSON (CreateWorkspace {commandId = commandId , workspaceId = workspaceId  } ) = object [
            "commandId" .= commandId,
            "workspaceId" .= workspaceId,
            "commandName" .= createWorkspaceCommandName]
  toJSON _  = error "to handle..."

instance FromJSON GsdCommand where

  parseJSON (Object jsonObject) = do
               commandNameMaybe <- jsonObject .: "commandName"
               case commandNameMaybe of
                    Just (String commandName) | (Text.unpack commandName) == createWorkspaceCommandName ->
                      CreateWorkspace
                          <$> jsonObject .: "commandId"
                          <*> jsonObject .: "workspaceId"
                    Just (String unknownCommandName) -> error $ "Command unknown : " ++ Text.unpack unknownCommandName
                    _ -> error $ "Command name not provided"
  parseJSON _ =  error $ "Json format not expected"
