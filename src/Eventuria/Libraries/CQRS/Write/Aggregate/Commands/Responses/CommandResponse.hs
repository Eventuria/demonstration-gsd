{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse where

import Eventuria.Libraries.CQRS.Write.Aggregate.Core
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import Eventuria.Libraries.CQRS.Write.CommandConsumption.Transaction.CommandTransaction
import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId

type RejectionReason = String


data CommandResponse  = CommandSuccessfullyProcessed {aggregateId::AggregateId, commandId :: CommandId}
                      | CommandFailed { aggregateId::AggregateId, commandId :: CommandId, reason :: RejectionReason}  deriving (Show,Eq)


instance AggregateJoinable CommandResponse where
  getAggregateId CommandSuccessfullyProcessed { aggregateId } = aggregateId
  getAggregateId CommandFailed { aggregateId } = aggregateId


instance CommandJoinable CommandResponse where

  getCommandId CommandSuccessfullyProcessed { commandId} = commandId
  getCommandId CommandFailed { commandId} = commandId



toCommandResponse :: CommandTransaction writeModel -> CommandResponse
toCommandResponse CommandTransaction { result = CommandAccepted {} , .. } = CommandSuccessfullyProcessed {..}
toCommandResponse CommandTransaction { result = CommandRejected {..} , ..} = CommandFailed {..}