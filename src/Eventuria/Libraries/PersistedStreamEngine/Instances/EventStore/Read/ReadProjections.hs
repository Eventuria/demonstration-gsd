{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Read.ReadProjections where

import           Data.Aeson
import           Data.Maybe
import           Data.Function ((&))


import qualified Eventuria.Adapters.Streamly.Safe as StreamlySafe

import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import           Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId
import           Eventuria.Libraries.CQRS.Write.Aggregate.Events.Event
                 
                 
import           Eventuria.Libraries.CQRS.Read.StreamRepository

import           Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.EventStoreStream
import           Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Read.Streaming
import           Eventuria.Libraries.CQRS.Write.StreamRepository
import           Eventuria.Libraries.CQRS.Write.Serialization.CommandTransaction()
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.Transaction.CommandTransaction
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse



streamAllAggregateId :: AggregateIdStream EventStoreStream  ->  StreamAll AggregateId
streamAllAggregateId  = streamAll


getStreamAllEventsByAggregateId :: FromJSON writeModel =>
                                   GetCommandTransactionStream EventStoreStream writeModel ->
                                   GetStreamAll Event
getStreamAllEventsByAggregateId  getCommandTransactionStream = (\aggregateId ->
  (streamAll $ getCommandTransactionStream aggregateId)
      & StreamlySafe.concatMap (\PersistedItem{item = CommandTransaction {result} } ->
          case result of
             CommandRejected {} ->       StreamlySafe.yield Nothing
             CommandAccepted {events} -> StreamlySafe.fromList $ Right $ Just <$> events)
      & StreamlySafe.filter (\itemMaybe -> maybe (False) (\just -> True) itemMaybe)
      & StreamlySafe.map (\itemJustOnly -> fromJust $ itemJustOnly)
      & StreamlySafe.indexed
      & StreamlySafe.map (\(offset,event) -> PersistedItem {offset, item = event}))

getStreamAllCommandResponseByAggregateId :: FromJSON writeModel =>
                                   GetCommandTransactionStream EventStoreStream writeModel ->
                                   GetStreamAll CommandResponse
getStreamAllCommandResponseByAggregateId  getCommandTransactionStream = (\aggregateId ->
  (streamAll $ getCommandTransactionStream aggregateId)
    & StreamlySafe.map (\PersistedItem{item = CommandTransaction {result , snapshot = Snapshot {..}, ..} } ->
        case result of
           CommandRejected {..} ->  PersistedItem { offset , item = CommandFailed {..}}
           CommandAccepted {} ->    PersistedItem { offset , item = CommandSuccessfullyProcessed {..}}))

getStreamAllWriteModelByAggregateId :: FromJSON writeModel =>
                                   GetCommandTransactionStream EventStoreStream writeModel ->
                                   GetStreamAll (Maybe writeModel)
getStreamAllWriteModelByAggregateId  getCommandTransactionStream = (\aggregateId ->
    (streamAll $ getCommandTransactionStream aggregateId)
      & StreamlySafe.map (\PersistedItem{item = CommandTransaction {snapshot = Snapshot {offset,writeModelMaybe}} } ->
          PersistedItem { offset , item = writeModelMaybe}))

