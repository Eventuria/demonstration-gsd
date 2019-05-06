{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Eventuria.GSD.Write.CommandConsumer.Handling.Commands.RenameWorkspace where

import qualified Data.UUID.V4 as Uuid
import qualified Data.Time as Time


import           Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandHandlingResult
                 
import           Eventuria.GSD.Write.Model.Events.Event
import           Eventuria.GSD.Write.Model.WriteModel
import           Eventuria.GSD.Write.Model.Commands.Command

handle :: GsdWriteModel ->
           RenameWorkspace ->
           IO (CommandHandlingResult)
handle writeModel RenameWorkspace { commandId, workspaceId, workspaceNewName} = do
       createdOn <- Time.getCurrentTime
       eventId <- Uuid.nextRandom
       return $ CommandValidated [toEvent $ WorkspaceRenamed { eventId ,
                                                               createdOn,
                                                               workspaceId ,
                                                               workspaceNewName}]

