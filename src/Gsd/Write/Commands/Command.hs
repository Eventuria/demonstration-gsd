{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Gsd.Write.Commands.Command where

import Prelude hiding (lookup)
import Data.Aeson
import Data.Maybe
import Data.Text
import Gsd.Write.Core
import qualified Data.Map as Map
import Cqrs.Write.Aggregate.Commands.CommandId
import Cqrs.Write.Aggregate.Commands.Command
import qualified Cqrs.Write.Aggregate.Commands.Command as CommandModule
import Control.Lens
import Data.Aeson.Lens


data GsdCommand =  CreateWorkspace { commandId :: CommandId , workspaceId ::WorkspaceId , workspaceName :: Text }
                 | RenameWorkspace { commandId :: CommandId , workspaceId ::WorkspaceId , workspaceNewName :: Text } deriving Show
--                 | SetGoal  { commandId :: CommandId ,
--                              workspaceId ::WorkspaceId ,
--                              goalId :: GoalId ,
--                              goalDetails :: String}
--                 | QuestionGoal {  commandId :: CommandId ,
--                                   workspaceId ::WorkspaceId ,
--                                   goalId :: GoalId ,
--                                   questionId :: QuestionId ,
--                                   questionDetails :: String}
--                 | ActionizeOnQuestion {
--                                       commandId :: CommandId ,
--                                       workspaceId ::WorkspaceId ,
--                                       questionId :: QuestionId ,
--                                       actionId :: ActionId ,
--                                       actionDetails :: String}
--                 | ActionizeOnTheGoalDirectly {
--                                       commandId :: CommandId ,
--                                       workspaceId ::WorkspaceId ,
--                                       goalId :: GoalId ,
--                                       actionId :: ActionId ,
--                                       actionDetails :: String}

createWorkspaceCommandName :: String
createWorkspaceCommandName = "createWorkspace"

renameWorkspaceCommandName :: String
renameWorkspaceCommandName = "renameWorkspace"

isCreateWorkspaceCommand :: Command -> Bool
isCreateWorkspaceCommand command = (commandName $ commandHeader command) == createWorkspaceCommandName

isRenameWorkspaceCommand :: Command -> Bool
isRenameWorkspaceCommand command = (commandName $ commandHeader command) == renameWorkspaceCommandName

toCommand :: GsdCommand -> Command
toCommand  CreateWorkspace {commandId, workspaceId, workspaceName} =
  Command { commandHeader = CommandHeader { commandId, aggregateId = workspaceId , commandName = createWorkspaceCommandName } ,
            payload = Map.fromList [("workspaceName",  String workspaceName ) ] }
toCommand  RenameWorkspace {commandId, workspaceId, workspaceNewName} =
  Command { commandHeader = CommandHeader { commandId, aggregateId = workspaceId , commandName = renameWorkspaceCommandName } ,
            payload = Map.fromList [("workspaceNewName",  String workspaceNewName ) ] }


fromCommand :: Command -> GsdCommand
fromCommand command =
  case (commandName $ commandHeader command) of
    "createWorkspace" -> CreateWorkspace {commandId = CommandModule.commandId $ commandHeader command,
                                               workspaceId = aggregateId $ commandHeader command,
                                               workspaceName =  fromJust $ (fromJust $ (Map.lookup "workspaceName" (payload command))) ^? _String  }
    "renameWorkspace" -> RenameWorkspace {commandId = CommandModule.commandId $ commandHeader command,
                                                   workspaceId = aggregateId $ commandHeader command,
                                                   workspaceNewName =  fromJust $ (fromJust $ (Map.lookup "workspaceNewName" (payload command))) ^? _String  }
    _ -> error "error from event"


