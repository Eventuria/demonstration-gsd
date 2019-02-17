{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module CQRS.Write.CqrsWrite where

import CQRS.Write.StreamRepository
import CQRS.Write.Aggregate.Commands.Command
import CQRS.Write.Aggregate.Commands.CommandHeader

import CQRS.Write.Aggregate.Core

import PersistedStreamEngine.Interface.Read.Reading
import PersistedStreamEngine.Interface.Write.Writing

import CQRS.Write.Serialization.Command ()
import CQRS.Write.Serialization.AggregateId ()

import PersistedStreamEngine.Interface.Write.PersistenceResult
import CQRS.Write.PersistCommandResult


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
 case isStreamNotExist of
   Right (True) -> do
    persist aggregateIdStream $ getAggregateId command
    convertPersistResult <$> persist commandStream command
   Right (False) -> convertPersistResult <$> persist commandStream command
   Left error -> return $ FailedToPersist {reason = show error, ..}
  where
    convertPersistResult :: PersistenceResult -> PersistCommandResult
    convertPersistResult PersistenceSuccess {lastOffsetPersisted} =
      SuccessfullyPersisted {aggregateId,commandId,lastOffsetPersisted}
    convertPersistResult PersistenceFailure {reason} =
      FailedToPersist {aggregateId,commandId,reason}