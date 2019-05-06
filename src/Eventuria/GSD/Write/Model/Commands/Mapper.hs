{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Eventuria.GSD.Write.Model.Commands.Mapper where

import Prelude hiding (lookup)
import Data.Aeson
import Data.Maybe
import Data.Text
import Data.UUID

import qualified Data.Map as Map

import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandHeader
import Control.Lens
import Data.Aeson.Lens
import Control.Monad

import Eventuria.GSD.Write.Model.Commands.Command

toCommand :: GSDCommand  -> Command
toCommand (GSDCommand gsdCommand @ CreateWorkspaceRep (CreateWorkspace {commandId, workspaceId, workspaceName})) =
  Command { commandHeader = CommandHeader {
                              commandId,
                              aggregateId = workspaceId ,
                              commandName = getCommandName gsdCommand } ,
            payload = Map.fromList [("workspaceName",  String workspaceName ) ] }

toCommand (GSDCommand gsdCommand  @ RenameWorkspaceRep  (RenameWorkspace {commandId, workspaceId, workspaceNewName})) =
  Command { commandHeader = CommandHeader {
                              commandId,
                              aggregateId = workspaceId ,
                              commandName = getCommandName gsdCommand } ,
          payload = Map.fromList [("workspaceNewName",  String workspaceNewName ) ] }

toCommand (GSDCommand gsdCommand @ SetGoalRep (SetGoal {commandId, workspaceId, goalId,goalDescription })) =
  Command { commandHeader = CommandHeader {
                              commandId,
                              aggregateId = workspaceId ,
                              commandName = getCommandName gsdCommand } ,
            payload = Map.fromList [
              ("goalId",  String $ (pack.toString) goalId ),
              ("goalDescription",  String goalDescription ) ] }

toCommand (GSDCommand gsdCommand @ RefineGoalDescriptionRep (RefineGoalDescription {commandId, workspaceId, goalId,refinedGoalDescription })) =
  Command { commandHeader = CommandHeader {
                              commandId,
                              aggregateId = workspaceId ,
                              commandName = getCommandName gsdCommand } ,
            payload = Map.fromList [
              ("goalId",  String $ (pack.toString) goalId ),
              ("refinedGoalDescription",  String refinedGoalDescription ) ] }

toCommand (GSDCommand gsdCommand @ StartWorkingOnGoalRep (StartWorkingOnGoal {commandId, workspaceId, goalId })) =
  Command { commandHeader = CommandHeader {
                              commandId,
                              aggregateId = workspaceId ,
                              commandName = getCommandName gsdCommand } ,
            payload = Map.fromList [
              ("goalId",  String $ (pack.toString) goalId )] }

toCommand (GSDCommand gsdCommand @ PauseWorkingOnGoalRep (PauseWorkingOnGoal {commandId, workspaceId, goalId } )) =
  Command { commandHeader = CommandHeader {
                              commandId,
                              aggregateId = workspaceId ,
                              commandName = getCommandName gsdCommand } ,
            payload = Map.fromList [
              ("goalId",  String $ (pack.toString) goalId )] }

toCommand (GSDCommand gsdCommand @ NotifyGoalAccomplishmentRep (NotifyGoalAccomplishment {commandId, workspaceId, goalId } )) =
  Command { commandHeader = CommandHeader {
                              commandId,
                              aggregateId = workspaceId ,
                              commandName = getCommandName gsdCommand } ,
            payload = Map.fromList [
              ("goalId",  String $ (pack.toString) goalId )] }

toCommand (GSDCommand gsdCommand @ GiveUpOnGoalRep (GiveUpOnGoal {commandId, workspaceId, goalId,reason })) =
  Command { commandHeader = CommandHeader {
                              commandId,
                              aggregateId = workspaceId ,
                              commandName = getCommandName gsdCommand } ,
            payload = Map.fromList [
              ("goalId",  String $ (pack.toString) goalId ),
              ("reason",  String reason ) ] }

toCommand (GSDCommand gsdCommand @ ActionizeOnGoalRep (ActionizeOnGoal {commandId, workspaceId, goalId,actionId,actionDetails } ))=
  Command { commandHeader = CommandHeader {
                              commandId,
                              aggregateId = workspaceId ,
                              commandName = getCommandName gsdCommand } ,
            payload = Map.fromList [
              ("goalId",  String $ (pack.toString) goalId ),
              ("actionId",  String $ (pack.toString) actionId ),
              ("actionDetails",  String actionDetails ) ] }

toCommand (GSDCommand gsdCommand @ NotifyActionCompletedRep (NotifyActionCompleted {commandId, workspaceId, goalId,actionId } )) =
  Command { commandHeader = CommandHeader {
                              commandId,
                              aggregateId = workspaceId ,
                              commandName = getCommandName gsdCommand } ,
            payload = Map.fromList [
              ("goalId",  String $ (pack.toString) goalId ),
              ("actionId",  String $ (pack.toString) actionId ) ] }


fromCommand :: Command -> GSDCommand
fromCommand Command { payload , commandHeader = CommandHeader {commandName,commandId,aggregateId = workspaceId}} =
  case (commandName) of
    "createWorkspace" -> GSDCommand
                            CreateWorkspaceRep
                            CreateWorkspace {commandId, workspaceId , workspaceName = (extractPayloadTextValue payload "workspaceName")}
    "renameWorkspace" -> GSDCommand
                            RenameWorkspaceRep
                            RenameWorkspace {commandId, workspaceId, workspaceNewName =  extractPayloadTextValue payload "workspaceNewName" }
    "setGoal" -> GSDCommand
                  SetGoalRep
                  SetGoal {commandId,
                          workspaceId,
                          goalId =  extractPayloadUUIDValue payload "goalId" ,
                          goalDescription =  extractPayloadTextValue payload "goalDescription"   }
    "refineGoalDescription" -> GSDCommand
                                 RefineGoalDescriptionRep
                                 RefineGoalDescription {
                                     commandId,
                                     workspaceId,
                                     goalId =  extractPayloadUUIDValue payload "goalId" ,
                                     refinedGoalDescription =  extractPayloadTextValue payload "refinedGoalDescription"   }
    "startWorkingOnGoal" -> GSDCommand
                             StartWorkingOnGoalRep
                             StartWorkingOnGoal {
                              commandId,
                              workspaceId,
                              goalId =  extractPayloadUUIDValue payload "goalId"}
    "pauseWorkingOnGoal" -> GSDCommand
                             PauseWorkingOnGoalRep
                             PauseWorkingOnGoal {
                              commandId,
                              workspaceId,
                              goalId =  extractPayloadUUIDValue payload "goalId"}
    "notifyGoalAccomplishment" -> GSDCommand
                                   NotifyGoalAccomplishmentRep
                                   NotifyGoalAccomplishment {
                                    commandId,
                                    workspaceId,
                                    goalId =  extractPayloadUUIDValue payload "goalId"}
    "giveUpOnGoal" -> GSDCommand
                       GiveUpOnGoalRep
                       GiveUpOnGoal {
                          commandId,
                          workspaceId,
                          goalId =  extractPayloadUUIDValue payload "goalId" ,
                          reason =  extractPayloadTextValue payload "reason"   }
    "actionizeOnGoal" -> GSDCommand
                          ActionizeOnGoalRep
                          ActionizeOnGoal {
                            commandId,
                            workspaceId,
                            goalId =  extractPayloadUUIDValue payload "goalId" ,
                            actionId =  extractPayloadUUIDValue payload "actionId" ,
                            actionDetails =  extractPayloadTextValue payload "actionDetails"   }
    "notifyActionCompleted" -> GSDCommand
                                NotifyActionCompletedRep
                                NotifyActionCompleted {
                                  commandId,
                                  workspaceId,
                                  goalId =  extractPayloadUUIDValue payload "goalId" ,
                                  actionId =  extractPayloadUUIDValue payload "actionId" }
    _ -> error "error from event"


extractPayloadTextValue :: CommandPayload -> String -> Text
extractPayloadTextValue payload key = fromJust $ (fromJust $ (Map.lookup key payload)) ^? _String

extractPayloadUUIDValue :: CommandPayload -> String -> UUID
extractPayloadUUIDValue payload key = fromJust $ join $ (fromString . unpack) <$> (fromJust $ (Map.lookup key payload)) ^? _String