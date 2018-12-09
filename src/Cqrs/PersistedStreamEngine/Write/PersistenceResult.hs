module Cqrs.PersistedStreamEngine.Write.PersistenceResult where

import Cqrs.PersistedStreamEngine.Offset

data PersistenceResult = PersistenceFailure {reason :: String} | PersistenceSuccess {lastOffsetPersisted :: Offset}

