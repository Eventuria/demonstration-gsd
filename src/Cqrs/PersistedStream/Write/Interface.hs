{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
module Cqrs.PersistedStream.Write.Interface where

import Cqrs.Streams
import Cqrs.PersistedStream.Write.Writable

data Writing persistedStream = Writing { persist :: forall item . Writable item =>  persistedStream item -> item -> IO (Either PersistenceFailure PersistResult) }

