
module Plugins.EventStore.Write.CqrsInstance where

import Cqrs.PersistedStreamEngine.Write.Interface
import qualified Plugins.EventStore.Write.Persisting as EventStoreWriting

import Plugins.EventStore.EventStoreStream

getEventStoreWriting :: Writing EventStoreStream
getEventStoreWriting = Writing {persist = EventStoreWriting.persist}