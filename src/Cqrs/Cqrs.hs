{-# LANGUAGE NamedFieldPuns #-}
module Cqrs.Cqrs where

import Cqrs.PersistedStreamEngine.Repository
import Cqrs.Aggregate.Commands.Command

import Cqrs.Aggregate.Core
import Cqrs.PersistedStreamEngine.Read.Interface
import Cqrs.PersistedStreamEngine.Write.Interface

import Cqrs.Serialization.Aggregate.Command ()
import Cqrs.Serialization.Aggregate.AggregateId ()

import Cqrs.PersistedStreamEngine.Write.PersistenceResult

persistCommands :: Writing persistedStream -> Querying persistedStream -> GetCommandStream persistedStream -> AggregateIdStream persistedStream -> Command -> IO PersistenceResult
persistCommands Writing {persist} Querying {isStreamNotFound} getCommandStream  aggregateIdStream  command = do
 let commandStream = getCommandStream $ getAggregateId command
 isStreamNotExist <- isStreamNotFound commandStream
 if(isStreamNotExist) then do
   persist aggregateIdStream $ getAggregateId command
   persist commandStream command
 else persist commandStream command
