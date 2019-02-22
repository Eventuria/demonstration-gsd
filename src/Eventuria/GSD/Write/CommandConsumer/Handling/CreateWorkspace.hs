{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Eventuria.GSD.Write.CommandConsumer.Handling.CreateWorkspace where


import Eventuria.Libraries.CQRS.EDsl
import Eventuria.GSD.Write.Model.Events.Event
import Eventuria.GSD.Write.Model.State
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import Eventuria.GSD.Write.Model.Core
import Data.Text
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
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