{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NamedFieldPuns #-}
module Cqrs.Write.CommandConsumption.Main  where

import Streamly (parallely)
import Data.Function ((&))
import Control.Concurrent

import Logger.Core
import Cqrs.Write.StreamRepository
import PersistedStreamEngine.Interface.Read.Reading

import Cqrs.Write.Serialization.Command ()
import Cqrs.Write.Serialization.ValidationState ()
import qualified Streamly.Safe as StreamlySafe
import System.SafeResponse
import Control.Exception
import Cqrs.Write.CommandConsumption.Core


execute ::  Logger ->
            AggregateIdStream persistedStreamEngine  ->
            Streaming persistedStreamEngine ->
            ConsumeAnAggregate ->
            IO (SafeResponse ())
execute logger
       aggregateIdStream
       Streaming {streamAllInfinitely}
       consumeAnAggregate = do
  threadId <-  myThreadId
  logInfo logger "runnning command consummers on all aggregates"

  try $ StreamlySafe.runStreamOnIOAndThrowFailureTo threadId
      $ parallely $
          streamAllInfinitely aggregateIdStream &
          StreamlySafe.mapM consumeAnAggregate








