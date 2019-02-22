{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.Libraries.CQRS.Write.CqrsWrite where

import Eventuria.Libraries.CQRS.Write.StreamRepository
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandHeader

import Eventuria.Libraries.CQRS.Write.Aggregate.Core

import Eventuria.Libraries.PersistedStreamEngine.Interface.Read.Reading
import Eventuria.Libraries.PersistedStreamEngine.Interface.Write.Writing

import Eventuria.Libraries.CQRS.Write.Serialization.Command ()
import Eventuria.Libraries.CQRS.Write.Serialization.AggregateId ()

import Eventuria.Libraries.PersistedStreamEngine.Interface.Write.PersistenceResult
import Eventuria.Libraries.CQRS.Write.PersistCommandResult


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