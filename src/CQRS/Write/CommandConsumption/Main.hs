{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NamedFieldPuns #-}
module CQRS.Write.CommandConsumption.Main  where

import Streamly (parallely)
import Data.Function ((&))
import Control.Concurrent

import Logger.Core
import CQRS.Write.StreamRepository
import PersistedStreamEngine.Interface.Read.Reading

import CQRS.Write.Serialization.Command ()
import CQRS.Write.Serialization.ValidationState ()
import qualified Streamly.Safe as StreamlySafe
import System.SafeResponse
import Control.Exception
import CQRS.Write.CommandConsumption.Core


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








