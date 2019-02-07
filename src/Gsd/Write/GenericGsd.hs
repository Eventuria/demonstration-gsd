{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Gsd.Write.GenericGsd  where

import Logger.Core

import qualified Cqrs.Write.CommandConsumption.Main as Cqrs.Write.CommandConsumption
import qualified Cqrs.Write.CqrsWrite as Cqrs.Write
import Cqrs.Write.StreamRepository

import PersistedStreamEngine.Interface.Write.Writing
import PersistedStreamEngine.Interface.Read.Reading
import PersistedStreamEngine.Interface.Write.WDsl

import Gsd.Write.Commands.Handling.CommandHandler (commandHandler)
import Gsd.Write.Commands.Command
import Gsd.Write.State
import Cqrs.Write.PersistCommandResult
import Cqrs.Write.Aggregate.Commands.Responses.CommandResponse
import PersistedStreamEngine.Interface.PersistedItem
import Cqrs.Write.Aggregate.Ids.AggregateId
import PersistedStreamEngine.Interface.Offset
import Cqrs.Write.Aggregate.Commands.CommandId
import Cqrs.Write.Serialization.CommandResponse()
import System.SafeResponse

persistCommand ::  AggregateIdStream persistedStream ->
                   GetCommandStream persistedStream->
                   Querying persistedStream ->
                   Writing persistedStream ->
                   GsdCommand ->
                   IO PersistCommandResult
persistCommand aggregateIdStream getCommandStream querying writing gsdCommand =
  Cqrs.Write.persistCommand
    writing
    querying
    getCommandStream
    aggregateIdStream $ toCommand gsdCommand

streamCommandConsumption :: CqrsStreamRepository persistedStream GsdState ->
                            Reading persistedStream ->
                            InterpreterWritePersistedStreamLanguage persistedStream GsdState () ->
                            Logger ->
                            IO (SafeResponse ())
streamCommandConsumption cqrsStreamRepository reading interpreterWritePersistedStreamLanguage logger  =
   Cqrs.Write.CommandConsumption.stream
      logger
      cqrsStreamRepository
      reading
      commandHandler
      interpreterWritePersistedStreamLanguage


waitTillCommandResponseProduced ::
                     GetCommandResponseStream persistedStream ->
                     Subscribing persistedStream ->
                     AggregateId ->
                     Offset ->
                     CommandId ->
                     IO (SafeResponse (Persisted CommandResponse))
waitTillCommandResponseProduced getCommandResponseStream subscribing @ Subscribing {subscribeOnOffset} aggregateId offset commandId =
    (subscribeOnOffset (getCommandResponseStream aggregateId) offset)




