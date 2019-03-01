{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NamedFieldPuns #-}
module Eventuria.Libraries.CQRS.Write.CommandConsumption.Main  where

import           Control.Concurrent
import           Control.Exception

import           Data.Function ((&))

import           Streamly (parallely)

import           Eventuria.Commons.Logger.Core

import qualified Eventuria.Adapters.Streamly.Safe as StreamlySafe

import           Eventuria.Libraries.PersistedStreamEngine.Interface.Read.Reading

import           Eventuria.Libraries.CQRS.Write.StreamRepository
import           Eventuria.Libraries.CQRS.Write.Serialization.Command ()
import           Eventuria.Libraries.CQRS.Write.Serialization.ValidationState ()
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.Core




execute ::  Logger ->
            AggregateIdStream persistedStreamEngine  ->
            Streaming persistedStreamEngine ->
            ConsumeAnAggregate ->
            IO (Either SomeException ())
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








