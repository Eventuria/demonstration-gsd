{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Gsd.Write.Commands.Command where

import Prelude hiding (lookup)
import Data.Aeson
import Data.Maybe
import Data.Text
import Data.UUID
import Gsd.Write.Core
import qualified Data.Map as Map
import Cqrs.Write.Aggregate.Commands.CommandId
import Cqrs.Write.Aggregate.Commands.Command
import Cqrs.Write.Aggregate.Commands.CommandHeader
import Control.Lens
import Data.Aeson.Lens
import Control.Monad
import GHC.Generics
data GsdCommand =  CreateWorkspace { commandId :: CommandId , workspaceId ::WorkspaceId , workspaceName :: Text }
                 | RenameWorkspace { commandId :: CommandId , workspaceId ::WorkspaceId , workspaceNewName :: Text }
                 | SetGoal  { commandId :: CommandId ,
                              workspaceId ::WorkspaceId ,
                              goalId :: GoalId ,
                              goalDescription :: Text}
                 | RefineGoalDescription { commandId :: CommandId ,
                                           workspaceId ::WorkspaceId ,
                                           goalId :: GoalId ,
                                           refinedGoalDescription :: Text}
                 | StartWorkingOnGoal {commandId :: CommandId ,
                                       workspaceId ::WorkspaceId ,
                                       goalId :: GoalId }
                 | PauseWorkingOnGoal {commandId :: CommandId ,
                                       workspaceId ::WorkspaceId ,
                                       goalId :: GoalId }
                 | NotifyGoalAccomplishment {commandId :: CommandId ,
                                             workspaceId ::WorkspaceId ,
                                             goalId :: GoalId }
                 | GiveUpOnGoal { commandId :: CommandId ,
                                 workspaceId ::WorkspaceId ,
                                 goalId :: GoalId ,
                                 reason :: Text} deriving (Show,Generic,Eq)
--                 | ActionizeOnTheGoalDirectly {
--                                       commandId :: CommandId ,
--                                       workspaceId ::WorkspaceId ,
--                                       goalId :: GoalId ,
--                                       actionId :: ActionId ,
--                                       actionDetails :: String}

createWorkspaceCommandName :: String
renameWorkspaceCommandName :: String
setGoalCommandName :: String
refineGoalDescriptionCommandName :: String
startWorkingOnGoalCommandName :: String
pauseWorkingOnGoalCommandName :: String
notifyGoalAccomplishmentCommandName :: String
giveUpOnGoalCommandName :: String

createWorkspaceCommandName = "createWorkspace"
renameWorkspaceCommandName = "renameWorkspace"
setGoalCommandName = "setGoal"
refineGoalDescriptionCommandName = "refineGoalDescription"
startWorkingOnGoalCommandName = "startWorkingOnGoal"
pauseWorkingOnGoalCommandName = "pauseWorkingOnGoal"
notifyGoalAccomplishmentCommandName = "notifyGoalAccomplishment"
giveUpOnGoalCommandName = "giveUpOnGoal"

isCreateWorkspaceCommand :: Command -> Bool
isRenameWorkspaceCommand :: Command -> Bool
isGoalSetCommand :: Command -> Bool
isRefineGoalDescriptionCommand :: Command -> Bool
isStartWorkingOnGoalCommand :: Command -> Bool
isPauseWorkingOnGoalCommand :: Command -> Bool
isNotifyGoalAccomplishmentCommand :: Command -> Bool
isGiveUpOnGoalCommand :: Command -> Bool

isCreateWorkspaceCommand command = (commandName $ commandHeader command) == createWorkspaceCommandName
isRenameWorkspaceCommand command = (commandName $ commandHeader command) == renameWorkspaceCommandName
isGoalSetCommand command = (commandName $ commandHeader command) == setGoalCommandName
isRefineGoalDescriptionCommand command = (commandName $ commandHeader command) == refineGoalDescriptionCommandName
isStartWorkingOnGoalCommand command = (commandName $ commandHeader command) == startWorkingOnGoalCommandName
isPauseWorkingOnGoalCommand command = (commandName $ commandHeader command) == pauseWorkingOnGoalCommandName
isNotifyGoalAccomplishmentCommand command = (commandName $ commandHeader command) == notifyGoalAccomplishmentCommandName
isGiveUpOnGoalCommand command = (commandName $ commandHeader command) == giveUpOnGoalCommandName



toCommand :: GsdCommand -> Command
toCommand  CreateWorkspace {commandId, workspaceId, workspaceName} =
  Command { commandHeader = CommandHeader { commandId, aggregateId = workspaceId , commandName = createWorkspaceCommandName } ,
            payload = Map.fromList [("workspaceName",  String workspaceName ) ] }
toCommand  RenameWorkspace {commandId, workspaceId, workspaceNewName} =
  Command { commandHeader = CommandHeader { commandId, aggregateId = workspaceId , commandName = renameWorkspaceCommandName } ,
            payload = Map.fromList [("workspaceNewName",  String workspaceNewName ) ] }
toCommand  SetGoal {commandId, workspaceId, goalId,goalDescription } =
  Command { commandHeader = CommandHeader { commandId, aggregateId = workspaceId , commandName = setGoalCommandName } ,
            payload = Map.fromList [
              ("goalId",  String $ (pack.toString) goalId ),
              ("goalDescription",  String goalDescription ) ] }
toCommand  RefineGoalDescription {commandId, workspaceId, goalId,refinedGoalDescription } =
  Command { commandHeader = CommandHeader { commandId, aggregateId = workspaceId , commandName = refineGoalDescriptionCommandName } ,
            payload = Map.fromList [
              ("goalId",  String $ (pack.toString) goalId ),
              ("refinedGoalDescription",  String refinedGoalDescription ) ] }
toCommand  StartWorkingOnGoal {commandId, workspaceId, goalId } =
  Command { commandHeader = CommandHeader { commandId, aggregateId = workspaceId , commandName = startWorkingOnGoalCommandName } ,
            payload = Map.fromList [
              ("goalId",  String $ (pack.toString) goalId )] }
toCommand  PauseWorkingOnGoal {commandId, workspaceId, goalId } =
  Command { commandHeader = CommandHeader { commandId, aggregateId = workspaceId , commandName = pauseWorkingOnGoalCommandName } ,
            payload = Map.fromList [
              ("goalId",  String $ (pack.toString) goalId )] }
toCommand  NotifyGoalAccomplishment {commandId, workspaceId, goalId } =
  Command { commandHeader = CommandHeader { commandId, aggregateId = workspaceId , commandName = notifyGoalAccomplishmentCommandName } ,
            payload = Map.fromList [
              ("goalId",  String $ (pack.toString) goalId )] }
toCommand  GiveUpOnGoal {commandId, workspaceId, goalId,reason } =
  Command { commandHeader = CommandHeader { commandId, aggregateId = workspaceId , commandName = giveUpOnGoalCommandName } ,
            payload = Map.fromList [
              ("goalId",  String $ (pack.toString) goalId ),
              ("reason",  String reason ) ] }

fromCommand :: Command -> GsdCommand
fromCommand Command { payload , commandHeader = CommandHeader {commandName,commandId,aggregateId = workspaceId}} =
  case (commandName) of
    "createWorkspace" -> CreateWorkspace {commandId, workspaceId, workspaceName =  extractPayloadTextValue payload "workspaceName" }
    "renameWorkspace" -> RenameWorkspace {commandId, workspaceId, workspaceNewName =  extractPayloadTextValue payload "workspaceNewName" }
    "setGoal" -> SetGoal {commandId,
                          workspaceId,
                          goalId =  extractPayloadUUIDValue payload "goalId" ,
                          goalDescription =  extractPayloadTextValue payload "goalDescription"   }
    "refineGoalDescription" -> RefineGoalDescription {commandId,
                               workspaceId,
                               goalId =  extractPayloadUUIDValue payload "goalId" ,
                               refinedGoalDescription =  extractPayloadTextValue payload "refinedGoalDescription"   }
    "startWorkingOnGoal" -> StartWorkingOnGoal {commandId,
                              workspaceId,
                              goalId =  extractPayloadUUIDValue payload "goalId"}
    "pauseWorkingOnGoal" -> PauseWorkingOnGoal {commandId,
                              workspaceId,
                              goalId =  extractPayloadUUIDValue payload "goalId"}
    "notifyGoalAccomplishment" -> NotifyGoalAccomplishment {commandId,
                              workspaceId,
                              goalId =  extractPayloadUUIDValue payload "goalId"}
    "giveUpOnGoal" -> GiveUpOnGoal {commandId,
                          workspaceId,
                          goalId =  extractPayloadUUIDValue payload "goalId" ,
                          reason =  extractPayloadTextValue payload "reason"   }
    _ -> error "error from event"


extractPayloadTextValue :: CommandPayload -> String -> Text
extractPayloadTextValue payload key = fromJust $ (fromJust $ (Map.lookup key payload)) ^? _String

extractPayloadUUIDValue :: CommandPayload -> String -> UUID
extractPayloadUUIDValue payload key = fromJust $ join $ (fromString . unpack) <$> (fromJust $ (Map.lookup key payload)) ^? _String