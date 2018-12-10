{-# LANGUAGE NamedFieldPuns #-}
module Cqrs.Write.CqrsWrite where

import Cqrs.Write.StreamRepository
import Cqrs.Write.Aggregate.Commands.Command

import Cqrs.Write.Aggregate.Core
import PersistedStreamEngine.Read.Interface
import PersistedStreamEngine.Write.Interface

import Cqrs.Write.Serialization.Command ()
import Cqrs.Write.Serialization.AggregateId ()

import PersistedStreamEngine.Write.PersistenceResult

persistCommand :: Writing persistedStream -> Querying persistedStream -> GetCommandStream persistedStream -> AggregateIdStream persistedStream -> Command -> IO PersistenceResult
persistCommand Writing {persist} Querying {isStreamNotFound} getCommandStream  aggregateIdStream  command = do
 let commandStream = getCommandStream $ getAggregateId command
 isStreamNotExist <- isStreamNotFound commandStream
 if(isStreamNotExist) then do
   persist aggregateIdStream $ getAggregateId command
   persist commandStream command
 else persist commandStream command
