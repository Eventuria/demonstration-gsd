module PersistedStreamEngine.Interface.Write.PersistenceResult where

import PersistedStreamEngine.Interface.Offset

data PersistenceResult = PersistenceFailure {reason :: String} | PersistenceSuccess {lastOffsetPersisted :: Offset}

