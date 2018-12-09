{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
module Cqrs.PersistedStreamEngine.Write.Interface where


import Cqrs.PersistedStreamEngine.Write.Writable
import Cqrs.PersistedStreamEngine.Write.PersistenceResult

data Writing persistedStream = Writing { persist :: forall item . Writable item =>  persistedStream item -> item -> IO PersistenceResult }

