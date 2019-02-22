{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Gsd.Write.Flow.Sourcer.Service.Generic  where

import qualified CQRS.Write.CqrsWrite as CQRS.Write
import CQRS.Write.StreamRepository

import PersistedStreamEngine.Interface.Write.Writing
import PersistedStreamEngine.Interface.Read.Reading
import Gsd.Write.Model.Commands.Command
import CQRS.Write.PersistCommandResult
import CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import PersistedStreamEngine.Interface.PersistedItem
import CQRS.Write.Aggregate.Ids.AggregateId
import PersistedStreamEngine.Interface.Offset
import CQRS.Write.Aggregate.Commands.CommandId
import CQRS.Write.Serialization.CommandResponse()
import System.SafeResponse

persistCommand ::  AggregateIdStream persistedStream ->
                   GetCommandStream persistedStream->
                   Querying persistedStream ->
                   Writing persistedStream ->
                   GsdCommand ->
                   IO PersistCommandResult
persistCommand aggregateIdStream getCommandStream querying writing gsdCommand =
  CQRS.Write.persistCommand
    writing
    querying
    getCommandStream
    aggregateIdStream $ toCommand gsdCommand

waitTillCommandResponseProduced ::
                     GetCommandResponseStream persistedStream ->
                     Subscribing persistedStream ->
                     AggregateId ->
                     Offset ->
                     CommandId ->
                     IO (SafeResponse (Persisted CommandResponse))
waitTillCommandResponseProduced getCommandResponseStream subscribing @ Subscribing {subscribeOnOffset} aggregateId offset commandId =
    (subscribeOnOffset (getCommandResponseStream aggregateId) offset)




