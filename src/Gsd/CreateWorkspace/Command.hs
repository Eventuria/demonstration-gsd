{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Gsd.CreateWorkspace.Command where

import Cqrs.Core
import Cqrs.Command
import qualified Cqrs.Command as CommandModule
import Gsd.Core
import qualified Data.Aeson as Aeson

data CreateWorkspace = CreateWorkspace { commandId :: CommandId , workspaceId ::WorkspaceId } deriving (Show,Eq)

isCreateWorkspaceCommand :: Command -> Bool
isCreateWorkspaceCommand command = (commandName $ commandHeader command) == "createWorkspace"

toCommand :: CreateWorkspace -> Command
toCommand  CreateWorkspace {commandId = commandId, workspaceId = workspaceId} =
  Command { commandHeader = CommandHeader { commandId = commandId, aggregateId = workspaceId , commandName = "createWorkspace" } ,
            payload = []}

fromCommand :: Command -> Maybe CreateWorkspace
fromCommand command =
  case (commandName $ commandHeader command) of
    "createWorkspace" -> Just CreateWorkspace {commandId = CommandModule.commandId $ commandHeader command, workspaceId = aggregateId $ commandHeader command}
    _ -> Nothing




