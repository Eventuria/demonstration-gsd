{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Eventuria.GSD.Write.CommandConsumer.Handling.CommandHandler (commandHandler) where

import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem

import           Eventuria.Libraries.CQRS.Write.CommandConsumption.Handling.CommandHandler


import           Eventuria.GSD.Write.Model.WriteModel
import           Eventuria.GSD.Write.Model.Commands.Command
import           Eventuria.GSD.Write.CommandConsumer.Handling.CommandPredicates
import qualified Eventuria.GSD.Write.CommandConsumer.Handling.Commands.CreateWorkspace          as CreateWorkspace
import qualified Eventuria.GSD.Write.CommandConsumer.Handling.Commands.RenameWorkspace          as RenameWorkspace
import qualified Eventuria.GSD.Write.CommandConsumer.Handling.Commands.SetGoal                  as SetGoal
import qualified Eventuria.GSD.Write.CommandConsumer.Handling.Commands.RefineGoalDescription    as RefineGoalDescription
import qualified Eventuria.GSD.Write.CommandConsumer.Handling.Commands.StartWorkingOnGoal       as StartWorkingOnGoal
import qualified Eventuria.GSD.Write.CommandConsumer.Handling.Commands.PauseWorkingOnGoal       as PauseWorkingOnGoal
import qualified Eventuria.GSD.Write.CommandConsumer.Handling.Commands.NotifyGoalAccomplishment as NotifyGoalAccomplishment
import qualified Eventuria.GSD.Write.CommandConsumer.Handling.Commands.GiveUpOnGoal             as GiveUpOnGoal
import qualified Eventuria.GSD.Write.CommandConsumer.Handling.Commands.ActionizeOnGoal          as ActionizeOnGoal
import qualified Eventuria.GSD.Write.CommandConsumer.Handling.Commands.NotifyActionCompleted    as NotifyActionCompleted

type GSDCommandHandler  = Maybe GsdWriteModel ->
                         (Persisted GsdCommand) ->
                         IO (CommandHandlerResult GsdWriteModel)

commandHandler :: CommandHandler GsdWriteModel
commandHandler writeModelMaybe
               PersistedItem {offset , item = command }
  | (isFirstCommand offset) && (not . isCreateWorkspaceCommand) command = return $ rejectCommand writeModelMaybe "CreateWorkspace should be the first command"
  | otherwise =   gsdCommandHandler writeModelMaybe PersistedItem {offset , item = (fromCommand command) }

gsdCommandHandler :: GSDCommandHandler
gsdCommandHandler
        writeModelMaybe
        PersistedItem {offset , item = gsdCommand } =
  case (writeModelMaybe, gsdCommand) of
     (Nothing        ,CreateWorkspace          {commandId, workspaceId, workspaceName})                   -> CreateWorkspace.handle          offset            commandId workspaceId workspaceName
     (Just writeModel,RenameWorkspace          {commandId, workspaceId, workspaceNewName})                -> RenameWorkspace.handle          offset writeModel commandId workspaceId workspaceNewName
     (Just writeModel,SetGoal                  {commandId, workspaceId, goalId, goalDescription})         -> SetGoal.handle                  offset writeModel commandId workspaceId goalId goalDescription
     (Just writeModel,RefineGoalDescription    {commandId, workspaceId, goalId, refinedGoalDescription})  -> RefineGoalDescription.handle    offset writeModel commandId workspaceId goalId refinedGoalDescription
     (Just writeModel,StartWorkingOnGoal       {commandId, workspaceId, goalId})                          -> StartWorkingOnGoal.handle       offset writeModel commandId workspaceId goalId
     (Just writeModel,PauseWorkingOnGoal       {commandId, workspaceId, goalId})                          -> PauseWorkingOnGoal.handle       offset writeModel commandId workspaceId goalId
     (Just writeModel,NotifyGoalAccomplishment {commandId, workspaceId, goalId})                          -> NotifyGoalAccomplishment.handle offset writeModel commandId workspaceId goalId
     (Just writeModel,GiveUpOnGoal             {commandId, workspaceId, goalId, reason})                  -> GiveUpOnGoal.handle             offset writeModel commandId workspaceId goalId reason
     (Just writeModel,ActionizeOnGoal          {commandId, workspaceId, goalId, actionId, actionDetails}) -> ActionizeOnGoal.handle          offset writeModel commandId workspaceId goalId actionId actionDetails
     (Just writeModel,NotifyActionCompleted    {commandId, workspaceId, goalId, actionId})                -> NotifyActionCompleted.handle    offset writeModel commandId workspaceId goalId actionId
     (_            ,_)                                                                                    -> return $ rejectCommand writeModelMaybe "Scenario not handle"






