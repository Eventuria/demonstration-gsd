{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandTransaction where

import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem

import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandHeader

import Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandHandlingResult

import GHC.Generics

data CommandTransaction = CommandTransaction {
                              commandId :: CommandId,
                              commandOffset :: Offset ,
                              aggregateId :: AggregateId ,
                              commandHandlingResult :: CommandHandlingResult } deriving (Eq,Generic, Show)

toCommandTransaction :: (Persisted Command) ->
                        CommandHandlingResult ->
                        CommandTransaction
toCommandTransaction PersistedItem { offset, item = Command { commandHeader = commandHeaderProcessed@CommandHeader {..}}}
                     commandHandlingResult  =
  CommandTransaction {
      commandId = commandId,
      commandOffset = offset,
      aggregateId,
      commandHandlingResult }

