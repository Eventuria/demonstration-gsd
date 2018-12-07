module Cqrs.PersistedStream.Write.PersistenceResult where

import Cqrs.Streams

data PersistenceResult = PersistenceFailure {reason :: String} | PersistenceSuccess {lastOffsetPersisted :: Offset}

