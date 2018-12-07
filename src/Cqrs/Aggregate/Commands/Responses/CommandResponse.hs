{-# LANGUAGE DuplicateRecordFields #-}
module Cqrs.Aggregate.Commands.Responses.CommandResponse where



import Cqrs.Aggregate.Ids.AggregateId
import Cqrs.Aggregate.Commands.CommandId
import Cqrs.Aggregate.Core
import Cqrs.Aggregate.Commands.Command

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



