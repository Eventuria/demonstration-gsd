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
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandHandling.Definition
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse



streamAllAggregateId :: AggregateIdStream EventStoreStream  ->  StreamAll AggregateId
streamAllAggregateId  = streamAll


getStreamAllEventsByAggregateId :: GetCommandTransactionStream EventStoreStream ->
                                   GetStreamAll Event
getStreamAllEventsByAggregateId  getCommandTransactionStream = (\aggregateId ->
  (streamAll $ getCommandTransactionStream aggregateId)
      & StreamlySafe.concatMap (\PersistedItem{item = CommandTransaction {commandHandlingResult} } ->
          case commandHandlingResult of
             CommandRejected {} ->       StreamlySafe.yield Nothing
             CommandValidated {events} -> StreamlySafe.fromList $ Right $ Just <$> events)
      & StreamlySafe.filter (\itemMaybe -> maybe (False) (\just -> True) itemMaybe)
      & StreamlySafe.map (\itemJustOnly -> fromJust $ itemJustOnly)
      & StreamlySafe.indexed
      & StreamlySafe.map (\(offset,event) -> PersistedItem {offset, item = event}))

getStreamAllCommandResponseByAggregateId :: GetCommandTransactionStream EventStoreStream  ->
                                            GetStreamAll CommandResponse
getStreamAllCommandResponseByAggregateId  getCommandTransactionStream = (\aggregateId ->
  (streamAll $ getCommandTransactionStream aggregateId)
    & StreamlySafe.map (\PersistedItem{item = CommandTransaction {..} } ->
        case commandHandlingResult of
           CommandRejected {..} ->  PersistedItem { offset = commandOffset , item = CommandFailed {..}}
           CommandValidated {} ->    PersistedItem { offset = commandOffset , item = CommandSuccessfullyProcessed {..}}))

getStreamAllWriteModelByAggregateId :: FromJSON writeModel =>
                                   GetCommandTransactionStream EventStoreStream  ->
                                   ProjectWriteModel writeModel ->
                                   GetStreamAll (Maybe writeModel)
getStreamAllWriteModelByAggregateId  getCommandTransactionStream
                                     projectWriteModel = (\aggregateId ->
    (streamAll $ getCommandTransactionStream aggregateId)
      & StreamlySafe.mapM
          (\PersistedItem {item = CommandTransaction {..} } -> do
            writeModelMaybeResult <- StreamlySafe.foldx
                                    (\writeModelMaybe PersistedItem { item = CommandTransaction {commandHandlingResult}}  ->
                                        projectWriteModel writeModelMaybe commandHandlingResult )
                                    Nothing
                                    id
                                    (streamFromRangeInclusive
                                      (getCommandTransactionStream aggregateId)
                                      0
                                      commandOffset)
            return $ fmap
                      (\writeModelUpToDateMaybe -> PersistedItem { offset = commandOffset , item = writeModelUpToDateMaybe})
                      writeModelMaybeResult))
