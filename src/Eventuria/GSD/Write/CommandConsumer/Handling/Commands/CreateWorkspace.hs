{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Eventuria.GSD.Write.CommandConsumer.Handling.Commands.CreateWorkspace where

import           Data.Text
import qualified Data.UUID.V4 as Uuid
import qualified Data.Time as Time
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
                 
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.Handling.CommandHandler
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
                 
import           Eventuria.GSD.Write.Model.Events.Event
import           Eventuria.GSD.Write.Model.WriteModel
import           Eventuria.GSD.Write.Model.Core

handle :: Offset ->
          CommandId ->
          WorkspaceId  ->
          Text ->
          IO (CommandHandlerResult GsdWriteModel)
handle offset
       commandId
       workspaceId
       workspaceName = do
         createdOn <- Time.getCurrentTime
         eventIdWorkspaceCreated <- Uuid.nextRandom
         eventIdWorkspaceNamed <- Uuid.nextRandom
         return $ validateCommand
                   GsdWriteModel {goals = [] }
                   [toEvent $ WorkspaceCreated {
                               eventId = eventIdWorkspaceCreated ,
                               createdOn,
                               workspaceId},
                    toEvent $ WorkspaceNamed   {
                               eventId = eventIdWorkspaceNamed ,
                               createdOn,
                               workspaceId ,
                               workspaceName}]

