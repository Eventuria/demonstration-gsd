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
                                           refinedGoalDescription :: Text} deriving (Show,Generic,Eq)
--                 | NotifyGoalAccomplishment {commandId :: CommandId ,
--                                                workspaceId ::WorkspaceId ,
--                                                goalId :: GoalId }
--                 | GiveUpOnAGoal { commandId :: CommandId ,
--                                 workspaceId ::WorkspaceId ,
--                                 goalId :: GoalId ,
--                                 reason :: Text}
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

setGoalCommandName :: String
setGoalCommandName = "setGoal"

refineGoalDescriptionCommandName :: String
refineGoalDescriptionCommandName = "refineGoalDescription"

isCreateWorkspaceCommand :: Command -> Bool
isCreateWorkspaceCommand command = (commandName $ commandHeader command) == createWorkspaceCommandName

isRenameWorkspaceCommand :: Command -> Bool
isRenameWorkspaceCommand command = (commandName $ commandHeader command) == renameWorkspaceCommandName

isGoalSetCommand :: Command -> Bool
isGoalSetCommand command = (commandName $ commandHeader command) == setGoalCommandName

isRefineGoalDescriptionCommand :: Command -> Bool
isRefineGoalDescriptionCommand command = (commandName $ commandHeader command) == refineGoalDescriptionCommandName

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
    _ -> error "error from event"


extractPayloadTextValue :: CommandPayload -> String -> Text
extractPayloadTextValue payload key = fromJust $ (fromJust $ (Map.lookup key payload)) ^? _String

extractPayloadUUIDValue :: CommandPayload -> String -> UUID
extractPayloadUUIDValue payload key = fromJust $ join $ (fromString . unpack) <$> (fromJust $ (Map.lookup key payload)) ^? _String