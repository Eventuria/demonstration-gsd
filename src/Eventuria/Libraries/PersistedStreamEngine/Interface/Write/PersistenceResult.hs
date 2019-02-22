module Eventuria.Libraries.PersistedStreamEngine.Interface.Write.PersistenceResult where

import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset

data PersistenceResult =  PersistenceFailure {reason :: String}
                        | PersistenceSuccess {lastOffsetPersisted :: Offset} deriving Show

