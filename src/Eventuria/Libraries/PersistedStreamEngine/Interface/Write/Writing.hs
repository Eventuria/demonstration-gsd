{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
module Eventuria.Libraries.PersistedStreamEngine.Interface.Write.Writing where


import Eventuria.Libraries.PersistedStreamEngine.Interface.Write.Writable
import Eventuria.Libraries.PersistedStreamEngine.Interface.Write.PersistenceResult

data Writing persistedStream = Writing { persist :: forall item . Writable item =>  persistedStream item -> item -> IO PersistenceResult }

