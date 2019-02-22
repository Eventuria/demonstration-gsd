{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NamedFieldPuns #-}
module Eventuria.Libraries.CQRS.Write.CommandConsumption.Main  where

import Streamly (parallely)
import Data.Function ((&))
import Control.Concurrent

import Eventuria.Commons.Logger.Core
import Eventuria.Libraries.CQRS.Write.StreamRepository
import Eventuria.Libraries.PersistedStreamEngine.Interface.Read.Reading

import Eventuria.Libraries.CQRS.Write.Serialization.Command ()
import Eventuria.Libraries.CQRS.Write.Serialization.ValidationState ()
import qualified Eventuria.Adapters.Streamly.Safe as StreamlySafe
import Eventuria.Commons.System.SafeResponse
import Control.Exception
import Eventuria.Libraries.CQRS.Write.CommandConsumption.Core


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








