
module Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Write.CqrsInstance where

import Eventuria.Libraries.PersistedStreamEngine.Interface.Write.Writing
import qualified Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Write.Persisting as EventStoreWriting

import Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.EventStoreStream

getEventStoreWriting :: Writing EventStoreStream
getEventStoreWriting = Writing {persist = EventStoreWriting.persist}