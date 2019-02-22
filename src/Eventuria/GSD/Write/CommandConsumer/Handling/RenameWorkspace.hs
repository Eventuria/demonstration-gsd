{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Eventuria.GSD.Write.CommandConsumer.Handling.RenameWorkspace where


import Eventuria.Libraries.CQRS.EDsl
import Eventuria.GSD.Write.Model.Events.Event
import Eventuria.GSD.Write.Model.State
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import Eventuria.GSD.Write.Model.Core
import Data.Text
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
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