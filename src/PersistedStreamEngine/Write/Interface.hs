{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
module PersistedStreamEngine.Write.Interface where


import PersistedStreamEngine.Write.Writable
import PersistedStreamEngine.Write.PersistenceResult

data Writing persistedStream = Writing { persist :: forall item . Writable item =>  persistedStream item -> item -> IO PersistenceResult }

