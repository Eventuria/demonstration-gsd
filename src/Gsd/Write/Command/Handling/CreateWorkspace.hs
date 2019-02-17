{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Gsd.Write.Command.Handling.CreateWorkspace where


import Cqrs.EDsl
import Gsd.Write.Model.Events.Event
import Gsd.Write.Model.State
import Cqrs.Write.Aggregate.Commands.ValidationStates.ValidationState
import Gsd.Write.Model.Core
import Data.Text
import Cqrs.Write.Aggregate.Commands.CommandId
import PersistedStreamEngine.Interface.Offset
import Data.Set (fromList)

handle :: Offset -> CommandId -> WorkspaceId -> Text  -> CommandDirective GsdState
handle offset commandId workspaceId workspaceName =
  Validate $ do
      now <- getCurrentTime
      eventId <- getNewEventID
      persistEvent $ toEvent $ WorkspaceCreated {  eventId , createdOn = now, workspaceId}
      persistEvent $ toEvent $ WorkspaceNamed   {  eventId , createdOn = now, workspaceId , workspaceName}
      updateValidationState ValidationState {lastOffsetConsumed = offset ,
                                                                commandsProcessed = fromList [commandId] ,
                                                                aggregateId = workspaceId,
                                                                state = Nothing}