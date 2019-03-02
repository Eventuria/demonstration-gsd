{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
module Eventuria.GSD.Write.CommandConsumer.Handling.Commands.RenameWorkspace where

import Data.Text
import Data.Set

import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset

import Eventuria.Libraries.CQRS.Write.CommandConsumption.Handling.ResponseDSL
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId

import Eventuria.GSD.Write.Model.Events.Event
import Eventuria.GSD.Write.Model.State
import Eventuria.GSD.Write.Model.Core


handle :: Offset ->
          ValidationState GsdState ->
          CommandId ->
          WorkspaceId ->
          Text  ->
          CommandHandlingResponse GsdState
handle offset
      (ValidationState {commandsProcessed, aggregateId, state})
      commandId
      workspaceId
      workspaceNewName =
  ValidateCommandWithFollowingTransactionPayload $ do
     createdOn <- getCurrentTime
     eventId <- getNewEventID
     persistEvent $ toEvent $ WorkspaceRenamed   { eventId ,
                                                   createdOn,
                                                   workspaceId ,
                                                   workspaceNewName}
     updateValidationState ValidationState {lastOffsetConsumed = offset ,
                                             commandsProcessed = union commandsProcessed (fromList [commandId]) ,
                                             aggregateId,
                                             state }