{-# LANGUAGE DuplicateRecordFields #-}
module Gsd.Commands where

import Cqrs.Core
import Cqrs.Command
import qualified Cqrs.Command as CommandModule
import Gsd.Core


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


isCreateWorkspaceCommand :: Command -> Bool
isCreateWorkspaceCommand command = (commandName $ commandHeader command) == "createWorkspace"

toCommand :: GsdCommand -> Command
toCommand  CreateWorkspace {commandId = commandId, workspaceId = workspaceId} =
  Command { commandHeader = CommandHeader { commandId = commandId, aggregateId = workspaceId , commandName = "createWorkspace" } ,
            payload = []}

fromCommand :: Command -> Maybe GsdCommand
fromCommand command =
  case (commandName $ commandHeader command) of
    "createWorkspace" -> Just CreateWorkspace {commandId = CommandModule.commandId $ commandHeader command, workspaceId = aggregateId $ commandHeader command}
    _ -> Nothing
