module Cqrs.Cqrs where

import Cqrs.Aggregate.Commands.Command
import Cqrs.Streams
import Cqrs.EventStore.Streaming
import Cqrs.Aggregate.Core
import Cqrs.EventStore.Context
import Cqrs.EventStore.Writing
import Cqrs.Aggregate.Commands.CommandStream
import Cqrs.Aggregate.Ids.AggregateIdStream

persistCommands :: EventStoreContext -> Command -> IO (Either PersistenceFailure PersistResult)
persistCommands context command = do
  let commandStream = getCommandStream context $ getAggregateId command
  isStreamNotExist <- isStreamNotExistRequest commandStream
  if(isStreamNotExist) then do
    persist (getAggregateIdStream context) $ getAggregateId command
    persist commandStream command
  else persist commandStream command
