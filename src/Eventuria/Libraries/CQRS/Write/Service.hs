{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.Libraries.CQRS.Write.Service where

import Control.Exception
import Data.Either.Combinators

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
                  IO (Either SomeException PersistCommandResult)
persistCommand writing
               querying
               getCommandStream
               aggregateIdStream
               command  = do
 let commandStream = getCommandStream $ getAggregateId command
 isFirstCommandToPersist querying commandStream >>=
   either
    (\exception -> return $ Left exception)
    (\isFirstCommandPersisted ->
      if isFirstCommandPersisted
        then handleFirstCommandToPersist writing aggregateIdStream commandStream command
        else handleCommandToPersist writing commandStream command )

  where
    isFirstCommandToPersist :: Querying persistedStream ->
                               CommandStream persistedStream ->
                               IO (Either SomeException Bool)
    isFirstCommandToPersist Querying {isStreamNotFound} commandStream = isStreamNotFound commandStream

    handleFirstCommandToPersist :: Writing persistedStream->
                                   AggregateIdStream persistedStream ->
                                   CommandStream persistedStream ->
                                   Command ->
                                   IO (Either SomeException PersistCommandResult)
    handleFirstCommandToPersist Writing {persist}
                                aggregateIdStream
                                commandStream
                                command @ Command {commandHeader = CommandHeader {aggregateId,commandId}} = do
      persist aggregateIdStream aggregateId
      persist commandStream command >>=
        (\persistResult -> return $ mapRight (convertPersistResult command) persistResult)

    handleCommandToPersist :: Writing persistedStream ->
                              CommandStream persistedStream ->
                              Command ->
                              IO (Either SomeException PersistCommandResult)
    handleCommandToPersist Writing {persist}
                           commandStream
                           command  =
       persist commandStream command >>=
       (\persistResult -> return $ mapRight (convertPersistResult command) persistResult)

    convertPersistResult :: Command -> PersistenceResult -> PersistCommandResult
    convertPersistResult Command {commandHeader = CommandHeader {aggregateId,commandId}}
                         PersistenceResult {lastOffsetPersisted} =
      PersistCommandResult {aggregateId,commandId,lastOffsetPersisted}
