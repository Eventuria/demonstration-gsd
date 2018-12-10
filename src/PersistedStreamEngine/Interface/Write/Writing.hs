{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
module PersistedStreamEngine.Interface.Write.Writing where


import PersistedStreamEngine.Interface.Write.Writable
import PersistedStreamEngine.Interface.Write.PersistenceResult

data Writing persistedStream = Writing { persist :: forall item . Writable item =>  persistedStream item -> item -> IO PersistenceResult }

