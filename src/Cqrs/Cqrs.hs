{-# LANGUAGE NamedFieldPuns #-}
module Cqrs.Cqrs where

import Cqrs.Aggregate.StreamRepository
import Cqrs.Aggregate.Commands.Command
import Cqrs.Streams

import Cqrs.Aggregate.Core
import Cqrs.PersistedStream.Read.Interface
import Cqrs.PersistedStream.Write.Interface

persistCommands :: Writing persistedStream -> Querying persistedStream -> GetCommandStream persistedStream -> AggregateIdStream persistedStream -> Command -> IO (Either PersistenceFailure PersistResult)
persistCommands Writing {persist} Querying {isStreamNotFound} getCommandStream  aggregateIdStream  command = do
 let commandStream = getCommandStream $ getAggregateId command
 isStreamNotExist <- isStreamNotFound commandStream
 if(isStreamNotExist) then do
   persist aggregateIdStream $ getAggregateId command
   persist commandStream command
 else persist commandStream command
