{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Gsd.Write.Command.Handling.RenameWorkspace where


import Cqrs.EDsl
import Gsd.Write.Model.Events.Event
import Gsd.Write.Model.State
import Cqrs.Write.Aggregate.Commands.ValidationStates.ValidationState
import Gsd.Write.Model.Core
import Data.Text
import Cqrs.Write.Aggregate.Commands.CommandId
import PersistedStreamEngine.Interface.Offset
import Data.Set


handle :: Offset -> ValidationState GsdState -> CommandId -> WorkspaceId -> Text  -> CommandDirective GsdState
handle offset (ValidationState {commandsProcessed, aggregateId, state}) commandId workspaceId workspaceNewName =
  Validate $ do
     now <- getCurrentTime
     eventId <- getNewEventID
     persistEvent $ toEvent $ WorkspaceRenamed   {  eventId , createdOn = now, workspaceId , workspaceNewName}
     updateValidationState ValidationState {lastOffsetConsumed = offset ,
                                             commandsProcessed = union commandsProcessed (fromList [commandId]) ,
                                             aggregateId,
                                             state }