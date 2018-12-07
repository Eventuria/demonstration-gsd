{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
module Cqrs.PersistedStream.Write.Interface where


import Cqrs.PersistedStream.Write.Writable
import Cqrs.PersistedStream.Write.PersistenceResult

data Writing persistedStream = Writing { persist :: forall item . Writable item =>  persistedStream item -> item -> IO PersistenceResult }

