module Gsd.Gsd where

import Gsd.Commands
import Cqrs.Streams
import Cqrs.Aggregate.Ids.AggregateIdStream
import Cqrs.EventStore.Context

requestCommand :: EventStoreContext -> GsdCommand -> IO (Either PersistenceFailure PersistResult)
requestCommand eventStoreContext gsdCommand = persistCommands eventStoreContext $ toCommand gsdCommand