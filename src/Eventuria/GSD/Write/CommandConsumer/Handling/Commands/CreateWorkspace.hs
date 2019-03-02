{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Eventuria.GSD.Write.CommandConsumer.Handling.Commands.CreateWorkspace where

import Data.Text
import Data.Set (fromList)

import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset

import Eventuria.Libraries.CQRS.Write.CommandConsumption.Handling.ResponseDSL
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState

import Eventuria.GSD.Write.Model.Events.Event
import Eventuria.GSD.Write.Model.State
import Eventuria.GSD.Write.Model.Core

handle :: Offset ->
          CommandId ->
          WorkspaceId  ->
          Text ->
          CommandHandlingResponse GsdState
handle offset
       commandId
       workspaceId
       workspaceName =
  ValidateCommandWithFollowingTransactionPayload $ do
      createdOn <- getCurrentTime
      eventId <- getNewEventID
      persistEvent $ toEvent $ WorkspaceCreated {
                                eventId ,
                                createdOn,
                                workspaceId}
      persistEvent $ toEvent $ WorkspaceNamed   {
                                eventId ,
                                createdOn,
                                workspaceId ,
                                workspaceName}
      updateValidationState ValidationState {
                                lastOffsetConsumed = offset ,
                                commandsProcessed = fromList [commandId] ,
                                aggregateId = workspaceId,
                                state = Nothing}