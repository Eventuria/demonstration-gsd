module Cqrs.PersistedStream.Write.PersistenceResult where

import Cqrs.PersistedStream.Offset

data PersistenceResult = PersistenceFailure {reason :: String} | PersistenceSuccess {lastOffsetPersisted :: Offset}

