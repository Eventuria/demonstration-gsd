{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Cqrs.Write.CqrsWrite where

import Cqrs.Write.StreamRepository
import Cqrs.Write.Aggregate.Commands.Command
import Cqrs.Write.Aggregate.Commands.CommandHeader

import Cqrs.Write.Aggregate.Core

import PersistedStreamEngine.Interface.Read.Reading
import PersistedStreamEngine.Interface.Write.Writing

import Cqrs.Write.Serialization.Command ()
import Cqrs.Write.Serialization.AggregateId ()

import PersistedStreamEngine.Interface.Write.PersistenceResult
import Cqrs.Write.PersistCommandResult

persistCommand :: Writing persistedStream ->
                  Querying persistedStream ->
                  GetCommandStream persistedStream ->
                  AggregateIdStream persistedStream ->
                  Command ->
                  IO PersistCommandResult
persistCommand Writing {persist}
               Querying {isStreamNotFound}
               getCommandStream
               aggregateIdStream
               command @ Command {commandHeader = CommandHeader {aggregateId,commandId}}  = do
 let commandStream = getCommandStream $ getAggregateId command
 isStreamNotExist <- isStreamNotFound commandStream
 if(isStreamNotExist) then do
   persist aggregateIdStream $ getAggregateId command
   convertPersistResult <$> persist commandStream command
 else convertPersistResult <$> persist commandStream command
  where
    convertPersistResult :: PersistenceResult -> PersistCommandResult
    convertPersistResult PersistenceSuccess {lastOffsetPersisted} =
      SuccessfullyPersisted {aggregateId,commandId,lastOffsetPersisted}
    convertPersistResult PersistenceFailure {reason} =
      FailedToPersist {aggregateId,commandId,reason}