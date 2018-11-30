{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module Cqrs.EventStore.Read.RDsl where

--import EventStore.Read.PersistedItem
--import EventStore.Read.Streaming
--import Control.Monad.Free
--import Cqrs.Aggregate.Ids.AggregateId
--import Streamly

--type StreamingItem stream item = Streamable stream item => stream IO (Persisted item)


--
--data Directive a = StreamAllAggregateIdInfinitelyParallely ( StreamingItem ParallelT AggregateId -> a)
--                 deriving (Functor)

--type ReadEventStoreLanguage a = Free Directive a

--streamAllInfinitely :: ReadEventStoreLanguage ()
--streamAllInfinitely eventType = Free (StreamAllInfinitely eventType (Pure ()))


