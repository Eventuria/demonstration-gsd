
module PersistedStreamEngine.Instances.EventStore.Write.CqrsInstance where

import PersistedStreamEngine.Interface.Write.Writing
import qualified PersistedStreamEngine.Instances.EventStore.Write.Persisting as EventStoreWriting

import PersistedStreamEngine.Instances.EventStore.EventStoreStream

getEventStoreWriting :: Writing EventStoreStream
getEventStoreWriting = Writing {persist = EventStoreWriting.persist}