module Eventuria.Libraries.PersistedStreamEngine.Interface.Write.PersistenceResult where

import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset

data PersistenceResult =  PersistenceResult {lastOffsetPersisted :: Offset} deriving Show

