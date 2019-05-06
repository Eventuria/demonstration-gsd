{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module Eventuria.GSD.Write.CommandConsumer.Handling.Commands.CreateWorkspace where

import qualified Data.UUID.V4 as Uuid
import qualified Data.Time as Time

import           Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandHandlingResult
                 
import           Eventuria.GSD.Write.Model.Events.Event
import           Eventuria.GSD.Write.Model.Commands.Command

handle :: CreateWorkspace -> IO (CommandHandlingResult)
handle CreateWorkspace { commandId, workspaceId, workspaceName} = do
         createdOn <- Time.getCurrentTime
         eventIdWorkspaceCreated <- Uuid.nextRandom
         eventIdWorkspaceNamed <- Uuid.nextRandom
         return $ CommandValidated
                    [toEvent $ WorkspaceCreated {
                               eventId = eventIdWorkspaceCreated ,
                               createdOn,
                               workspaceId},
                    toEvent $ WorkspaceNamed   {
                               eventId = eventIdWorkspaceNamed ,
                               createdOn,
                               workspaceId ,
                               workspaceName}]


