{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Eventuria.GSD.Write.Flow.Sourcer.Service.Generic  where

import qualified Eventuria.Libraries.CQRS.Write.CqrsWrite as Eventuria.Libraries.CQRS.Write
import Eventuria.Libraries.CQRS.Write.StreamRepository

import Eventuria.Libraries.PersistedStreamEngine.Interface.Write.Writing
import Eventuria.Libraries.PersistedStreamEngine.Interface.Read.Reading
import Eventuria.GSD.Write.Model.Commands.Command
import Eventuria.Libraries.CQRS.Write.PersistCommandResult
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId
import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
import Eventuria.Libraries.CQRS.Write.Serialization.CommandResponse()
import Eventuria.Commons.System.SafeResponse

persistCommand ::  AggregateIdStream persistedStream ->
                   GetCommandStream persistedStream->
                   Querying persistedStream ->
                   Writing persistedStream ->
                   GsdCommand ->
                   IO PersistCommandResult
persistCommand aggregateIdStream getCommandStream querying writing gsdCommand =
  Eventuria.Libraries.CQRS.Write.persistCommand
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




