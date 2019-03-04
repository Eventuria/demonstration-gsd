{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Eventuria.Libraries.CQRS.Write.CommandConsumption.Transaction.CommandTransaction where

import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
import Eventuria.Libraries.CQRS.Write.Aggregate.Events.Event
import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId
import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandHeader
import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import Eventuria.Libraries.CQRS.Write.CommandConsumption.Handling.CommandHandler

import GHC.Generics


data CommandProcessResult  = CommandAccepted { events :: [Event]}
                           | CommandRejected { reason :: RejectionReason}  deriving (Show,Eq)

data CommandTransaction writeModel  = CommandTransaction {
                              commandId :: CommandId,
                              aggregateId :: AggregateId ,
                              snapshot :: Snapshot writeModel,
                              result :: CommandProcessResult } deriving (Eq,Generic, Show)


data Snapshot writeModel = Snapshot {
                                offset :: Offset ,
                                writeModelMaybe :: Maybe writeModel } deriving (Eq,Generic, Show)


toCommandTransaction :: (Persisted Command) ->
                        CommandHandlerResult writeModel ->
                        CommandTransaction writeModel
toCommandTransaction PersistedItem { offset, item = Command { commandHeader = commandHeaderProcessed@CommandHeader {..}}}
                     CommandHandlerResult { writeModelMaybe, result = (RejectCommand rejectionReason) } =
  CommandTransaction {
      commandId = commandId,
      aggregateId,
      snapshot = Snapshot { offset,writeModelMaybe},
      result = CommandRejected {reason = rejectionReason}}
toCommandTransaction PersistedItem { offset, item = Command { commandHeader = commandHeaderProcessed@CommandHeader {..}}}
                     CommandHandlerResult { writeModelMaybe, result = (ValidateCommand events) } =
  CommandTransaction {
     commandId = commandId,
     aggregateId,
     snapshot = Snapshot { offset,writeModelMaybe},
     result = CommandAccepted {events}}
