{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NamedFieldPuns #-}
module Eventuria.Libraries.CQRS.Write.CommandConsumption.Service  where

import           Control.Concurrent
import           Control.Exception

import           Data.Function ((&))

import           Streamly (parallely)

import           Eventuria.Commons.Logger.Core

import qualified Eventuria.Adapters.Streamly.Safe as StreamlySafe

import           Eventuria.Libraries.PersistedStreamEngine.Interface.Read.Reading

import           Eventuria.Libraries.CQRS.Write.StreamRepository
import           Eventuria.Libraries.CQRS.Write.Serialization.Command ()
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.Definitions




startCommandConsumption ::  Logger ->
            AggregateIdStream persistedStreamEngine  ->
            Streaming persistedStreamEngine ->
            OrchestratreCommandConsumptionForAggregate writeModel ->
            IO (Either SomeException ())
startCommandConsumption  logger
                         aggregateIdStream
                         Streaming {streamAllInfinitely}
                         orchestratreCommandConsumptionForAggregate = do
  threadId <-  myThreadId
  logInfo logger "runnning command consummers on all aggregates"

  try $ StreamlySafe.runStreamOnIOAndThrowFailureTo threadId
        $ parallely
        $ streamAllInfinitely aggregateIdStream & StreamlySafe.mapM
             (\persistedAggregateId -> try $ StreamlySafe.runStreamOnIOAndThrowFailureTo
                                        threadId
                                        (orchestratreCommandConsumptionForAggregate persistedAggregateId))








