module Cqrs.Cqrs where

import Cqrs.Aggregate.StreamRepository
import Cqrs.Aggregate.Commands.Command
import Cqrs.Streams
import EventStore.Read.Streaming
import Cqrs.Aggregate.Core
import EventStore.Write.Persisting


persistCommands :: GetCommandStream -> AggregateIdStream -> Command -> IO (Either PersistenceFailure PersistResult)
persistCommands getCommandStream  aggregateIdStream command = do
 let commandStream = getCommandStream $ getAggregateId command
 isStreamNotExist <- isStreamNotExistRequest commandStream
 if(isStreamNotExist) then do
   persist aggregateIdStream $ getAggregateId command
   persist commandStream command
 else persist commandStream command
