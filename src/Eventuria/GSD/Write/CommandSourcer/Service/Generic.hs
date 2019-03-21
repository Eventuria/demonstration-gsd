{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Eventuria.GSD.Write.CommandSourcer.Service.Generic  where

import           Control.Exception

import           Eventuria.Libraries.PersistedStreamEngine.Interface.Write.Writing
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Read.Reading
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem

import qualified Eventuria.Libraries.CQRS.Write.Service as CQRS.Service
import           Eventuria.Libraries.CQRS.Write.StreamRepository
import           Eventuria.Libraries.CQRS.Write.PersistCommandResult
import           Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
import           Eventuria.Libraries.CQRS.Write.Serialization.CommandTransaction()
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandTransaction
import           Eventuria.GSD.Write.Model.Commands.Command

persistCommand ::  AggregateIdStream persistedStream ->
                   GetCommandStream persistedStream->
                   Querying persistedStream ->
                   Writing persistedStream ->
                   GsdCommand ->
                   IO ( Either SomeException PersistCommandResult)
persistCommand aggregateIdStream getCommandStream querying writing gsdCommand =
  CQRS.Service.persistCommand
    writing
    querying
    getCommandStream
    aggregateIdStream $ toCommand gsdCommand

waitTillCommandResponseProduced :: GetCommandTransactionStream persistedStream ->
                                   Subscribing persistedStream ->
                                   AggregateId ->
                                   Offset ->
                                   CommandId ->
                                   IO (Either SomeException (Persisted CommandTransaction))
waitTillCommandResponseProduced getCommandTransactionStream subscribing @ Subscribing {subscribeOnOffset} aggregateId offset commandId =
    (subscribeOnOffset (getCommandTransactionStream aggregateId) offset)




