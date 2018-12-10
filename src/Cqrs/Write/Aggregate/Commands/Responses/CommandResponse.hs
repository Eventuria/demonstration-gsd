{-# LANGUAGE DuplicateRecordFields #-}
module Cqrs.Write.Aggregate.Commands.Responses.CommandResponse where



import Cqrs.Write.Aggregate.Ids.AggregateId
import Cqrs.Write.Aggregate.Commands.CommandId
import Cqrs.Write.Aggregate.Core
import Cqrs.Write.Aggregate.Commands.Command

type RejectionReason = String

data CommandResponse  = CommandSuccessfullyProcessed { commandId :: CommandId , aggregateId :: AggregateId }
                      | CommandSkippedBecauseAlreadyProcessed { commandId :: CommandId , aggregateId ::AggregateId }
                      | CommandFailed { commandId :: CommandId , aggregateId ::AggregateId , reason :: RejectionReason}  deriving (Show,Eq)


instance AggregateJoinable CommandResponse where
  getAggregateId CommandSuccessfullyProcessed { aggregateId = aggregateId} = aggregateId
  getAggregateId CommandSkippedBecauseAlreadyProcessed { aggregateId = aggregateId} = aggregateId
  getAggregateId CommandFailed { aggregateId = aggregateId} = aggregateId


instance CommandJoinable CommandResponse where

  getCommandId CommandSuccessfullyProcessed { commandId = commandId} = commandId
  getCommandId CommandSkippedBecauseAlreadyProcessed { commandId = commandId} = commandId
  getCommandId CommandFailed { commandId = commandId} = commandId



