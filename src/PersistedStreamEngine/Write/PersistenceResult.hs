module PersistedStreamEngine.Write.PersistenceResult where

import PersistedStreamEngine.Offset

data PersistenceResult = PersistenceFailure {reason :: String} | PersistenceSuccess {lastOffsetPersisted :: Offset}

