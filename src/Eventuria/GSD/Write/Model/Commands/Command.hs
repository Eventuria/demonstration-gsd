{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
module Eventuria.GSD.Write.Model.Commands.Command where

import Data.Text
import GHC.Generics

import Eventuria.GSD.Write.Model.Core
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId



data CreateWorkspace = CreateWorkspace { commandId :: CommandId , workspaceId ::WorkspaceId , workspaceName :: Text }    deriving (Show,Generic,Eq)
data RenameWorkspace = RenameWorkspace { commandId :: CommandId , workspaceId ::WorkspaceId , workspaceNewName :: Text } deriving (Show,Generic,Eq)
data SetGoal =  SetGoal {
                   commandId :: CommandId ,
                   workspaceId ::WorkspaceId ,
                   goalId :: GoalId ,
                   goalDescription :: Text} deriving (Show,Generic,Eq)
data RefineGoalDescription = RefineGoalDescription {
                               commandId :: CommandId ,
                               workspaceId ::WorkspaceId ,
                               goalId :: GoalId ,
                               refinedGoalDescription :: Text} deriving (Show,Generic,Eq)
data StartWorkingOnGoal = StartWorkingOnGoal {
                               commandId :: CommandId ,
                               workspaceId ::WorkspaceId ,
                               goalId :: GoalId } deriving (Show,Generic,Eq)
data PauseWorkingOnGoal = PauseWorkingOnGoal {
                               commandId :: CommandId ,
                               workspaceId ::WorkspaceId ,
                               goalId :: GoalId } deriving (Show,Generic,Eq)
data NotifyGoalAccomplishment = NotifyGoalAccomplishment {
                                   commandId :: CommandId ,
                                   workspaceId ::WorkspaceId ,
                                   goalId :: GoalId } deriving (Show,Generic,Eq)
data GiveUpOnGoal = GiveUpOnGoal {
                       commandId :: CommandId ,
                       workspaceId ::WorkspaceId ,
                       goalId :: GoalId ,
                       reason :: Text} deriving (Show,Generic,Eq)
data ActionizeOnGoal = ActionizeOnGoal {
                         commandId :: CommandId ,
                         workspaceId ::WorkspaceId ,
                         goalId :: GoalId ,
                         actionId :: ActionId ,
                         actionDetails :: Text} deriving (Show,Generic,Eq)
data NotifyActionCompleted = NotifyActionCompleted {
                              commandId :: CommandId ,
                              workspaceId ::WorkspaceId ,
                              goalId :: GoalId ,
                              actionId :: ActionId } deriving (Show,Generic,Eq)

-- Dynamic Types Pattern
data GSDCommand where
  GSDCommand ::  Show commandType => GSDCommandRep commandType -> commandType -> GSDCommand


instance Show GSDCommand where
  show (GSDCommand _ command) = show command

instance Generic GSDCommand where
   (GSDCommand _ command) = show command


-- Typecase Pattern
data GSDCommandRep commandType where
  CreateWorkspaceRep          :: GSDCommandRep CreateWorkspace
  RenameWorkspaceRep          :: GSDCommandRep RenameWorkspace
  SetGoalRep                  :: GSDCommandRep SetGoal
  RefineGoalDescriptionRep    :: GSDCommandRep RefineGoalDescription
  StartWorkingOnGoalRep       :: GSDCommandRep StartWorkingOnGoal
  PauseWorkingOnGoalRep       :: GSDCommandRep PauseWorkingOnGoal
  NotifyGoalAccomplishmentRep :: GSDCommandRep NotifyGoalAccomplishment
  GiveUpOnGoalRep             :: GSDCommandRep GiveUpOnGoal
  ActionizeOnGoalRep          :: GSDCommandRep ActionizeOnGoal
  NotifyActionCompletedRep    :: GSDCommandRep NotifyActionCompleted

getCommandName :: GSDCommandRep commandType -> String
getCommandName command = case command of
   CreateWorkspaceRep          -> "createWorkspace"
   RenameWorkspaceRep          -> "renameWorkspace"
   SetGoalRep                  -> "setGoal"
   RefineGoalDescriptionRep    -> "refineGoalDescription"
   StartWorkingOnGoalRep       -> "startWorkingOnGoal"
   PauseWorkingOnGoalRep       -> "pauseWorkingOnGoal"
   NotifyGoalAccomplishmentRep -> "notifyGoalAccomplishment"
   GiveUpOnGoalRep             -> "giveUpOnGoal"
   ActionizeOnGoalRep          -> "actionizeOnGoal"
   NotifyActionCompletedRep    -> "notifyActionCompleted"













