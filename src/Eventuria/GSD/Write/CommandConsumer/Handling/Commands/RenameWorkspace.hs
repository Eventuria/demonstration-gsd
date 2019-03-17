{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Eventuria.GSD.Write.CommandConsumer.Handling.Commands.RenameWorkspace where

import           Data.Text
import qualified Data.UUID.V4 as Uuid
import qualified Data.Time as Time

import           Eventuria.Libraries.PersistedStreamEngine.Interface.Offset

import           Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandHandling.Definition
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
                 
import           Eventuria.GSD.Write.Model.Events.Event
import           Eventuria.GSD.Write.Model.WriteModel
import           Eventuria.GSD.Write.Model.Core


handle :: Offset ->
          GsdWriteModel ->
          CommandId ->
          WorkspaceId ->
          Text  ->
          IO (CommandHandlingResult)
handle offset
      writeModel
      commandId
      workspaceId
      workspaceNewName = do
       createdOn <- Time.getCurrentTime
       eventId <- Uuid.nextRandom
       return $ CommandValidated [toEvent $ WorkspaceRenamed { eventId ,
                                                               createdOn,
                                                               workspaceId ,
                                                               workspaceNewName}]
