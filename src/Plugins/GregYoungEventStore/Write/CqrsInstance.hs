
module Plugins.GregYoungEventStore.Write.CqrsInstance where

import Cqrs.PersistedStream.Write.Interface
import qualified Plugins.GregYoungEventStore.Write.Persisting as EventStoreWriting

import Plugins.GregYoungEventStore.Stream

getEventStoreWriting :: Writing EventStoreStream
getEventStoreWriting = Writing {persist = EventStoreWriting.persist}