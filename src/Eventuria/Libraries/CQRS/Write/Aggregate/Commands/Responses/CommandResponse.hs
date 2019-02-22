{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse where

import Eventuria.Libraries.CQRS.Write.Aggregate.Core
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandHeader

type RejectionReason = String

data CommandResponse  = CommandSuccessfullyProcessed { commandHeaderProcessed :: CommandHeader }
                      | CommandFailed { commandHeaderProcessed :: CommandHeader, reason :: RejectionReason}  deriving (Show,Eq)


instance AggregateJoinable CommandResponse where
  getAggregateId CommandSuccessfullyProcessed { commandHeaderProcessed = CommandHeader{aggregateId} } = aggregateId
  getAggregateId CommandFailed { commandHeaderProcessed = CommandHeader{aggregateId} } = aggregateId


instance CommandJoinable CommandResponse where

  getCommandId CommandSuccessfullyProcessed { commandHeaderProcessed = CommandHeader{commandId}} = commandId
  getCommandId CommandFailed { commandHeaderProcessed = CommandHeader{commandId}} = commandId



