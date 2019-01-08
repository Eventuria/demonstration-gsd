{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Cqrs.Write.Aggregate.Commands.Responses.CommandResponse where

import Cqrs.Write.Aggregate.Core
import Cqrs.Write.Aggregate.Commands.Command
import Cqrs.Write.Aggregate.Commands.CommandHeader

type RejectionReason = String

data CommandResponse  = CommandSuccessfullyProcessed { commandHeaderProcessed :: CommandHeader }
                      | CommandSkippedBecauseAlreadyProcessed { commandHeaderProcessed :: CommandHeader }
                      | CommandFailed { commandHeaderProcessed :: CommandHeader, reason :: RejectionReason}  deriving (Show,Eq)


instance AggregateJoinable CommandResponse where
  getAggregateId CommandSuccessfullyProcessed { commandHeaderProcessed = CommandHeader{aggregateId} } = aggregateId
  getAggregateId CommandSkippedBecauseAlreadyProcessed { commandHeaderProcessed = CommandHeader{aggregateId} } = aggregateId
  getAggregateId CommandFailed { commandHeaderProcessed = CommandHeader{aggregateId} } = aggregateId


instance CommandJoinable CommandResponse where

  getCommandId CommandSuccessfullyProcessed { commandHeaderProcessed = CommandHeader{commandId}} = commandId
  getCommandId CommandSkippedBecauseAlreadyProcessed { commandHeaderProcessed = CommandHeader{commandId}} = commandId
  getCommandId CommandFailed { commandHeaderProcessed = CommandHeader{commandId}} = commandId



