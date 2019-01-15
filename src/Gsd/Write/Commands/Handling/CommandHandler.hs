{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Gsd.Write.Commands.Handling.CommandHandler (commandHandler) where


import Gsd.Write.Commands.Command
import Gsd.Write.CommandPredicates

import Cqrs.Write.CommandConsumption.CommandHandler
import Cqrs.EDsl
import Cqrs.Write.Aggregate.Commands.ValidationStates.ValidationState
import Gsd.Write.State
import PersistedStreamEngine.Interface.PersistedItem
import qualified Gsd.Write.Commands.Handling.CreateWorkspace as CreateWorkspace
import qualified Gsd.Write.Commands.Handling.RenameWorkspace as RenameWorkspace
import qualified Gsd.Write.Commands.Handling.SetGoal as SetGoal
import qualified Gsd.Write.Commands.Handling.RefineGoalDescription as RefineGoalDescription
import qualified Gsd.Write.Commands.Handling.StartWorkingOnGoal as StartWorkingOnGoal
import qualified Gsd.Write.Commands.Handling.PauseWorkingOnGoal as PauseWorkingOnGoal
import qualified Gsd.Write.Commands.Handling.NotifyGoalAccomplishment as NotifyGoalAccomplishment
import qualified Gsd.Write.Commands.Handling.GiveUpOnGoal as GiveUpOnGoal

import PersistedStreamEngine.Interface.Offset

type GSDCommandHandler = Offset -> GsdCommand -> Maybe (ValidationState GsdState) -> CommandDirective GsdState

commandHandler :: CommandHandler GsdState
commandHandler persistedCommand@PersistedItem {offset , item = command } snapshotMaybe
  | isAlreadyProcessed offset snapshotMaybe = SkipBecauseAlreadyProcessed
  | (isFirstCommand offset) && (not . isCreateWorkspaceCommand) command = Reject "CreateWorkspace should be the first command"
  | otherwise =   gsdCommandHandler offset (fromCommand command)  snapshotMaybe

gsdCommandHandler :: GSDCommandHandler
gsdCommandHandler
        offset
        gsdCommand
        snapshotMaybe =  case (snapshotMaybe, gsdCommand) of
          (Nothing,CreateWorkspace {commandId, workspaceId, workspaceName}) -> CreateWorkspace.handle offset commandId workspaceId workspaceName
          (Just snapshot,RenameWorkspace {commandId, workspaceId, workspaceNewName}) -> RenameWorkspace.handle offset snapshot commandId workspaceId workspaceNewName
          (Just snapshot,SetGoal {commandId, workspaceId, goalId, goalDescription}) -> SetGoal.handle offset snapshot commandId workspaceId goalId goalDescription
          (Just snapshot,RefineGoalDescription {commandId, workspaceId, goalId, refinedGoalDescription}) -> RefineGoalDescription.handle offset snapshot commandId workspaceId goalId refinedGoalDescription
          (Just snapshot,StartWorkingOnGoal {commandId, workspaceId, goalId}) -> StartWorkingOnGoal.handle offset snapshot commandId workspaceId goalId
          (Just snapshot,PauseWorkingOnGoal {commandId, workspaceId, goalId}) -> PauseWorkingOnGoal.handle offset snapshot commandId workspaceId goalId
          (Just snapshot,NotifyGoalAccomplishment {commandId, workspaceId, goalId}) -> NotifyGoalAccomplishment.handle offset snapshot commandId workspaceId goalId
          (Just snapshot,GiveUpOnGoal {commandId, workspaceId, goalId, reason}) -> GiveUpOnGoal.handle offset snapshot commandId workspaceId goalId reason
          (_,_) -> Reject "Scenario not handle"






