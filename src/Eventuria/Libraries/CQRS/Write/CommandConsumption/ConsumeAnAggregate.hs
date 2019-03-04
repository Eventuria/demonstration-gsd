{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
module Eventuria.Libraries.CQRS.Write.CommandConsumption.ConsumeAnAggregate (getConsumeAnAggregate)where

import           Control.Concurrent
import           Control.Exception

import           Data.Aeson
import           Data.Function ((&))
import           Data.Maybe

import           Eventuria.Commons.Logger.Core

import qualified Eventuria.Adapters.Streamly.Safe as StreamlySafe


import           Eventuria.Libraries.PersistedStreamEngine.Interface.Write.Writing
import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Read.Reading
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Streamable

import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import           Eventuria.Libraries.CQRS.Write.StreamRepository
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.Handling.CommandHandler
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.Definitions

import           Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId

import           Eventuria.Libraries.CQRS.Write.Serialization.Command ()
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.Transaction.CommandTransaction
import           Eventuria.Libraries.CQRS.Write.Serialization.CommandTransaction ()

getConsumeAnAggregate :: (FromJSON writeModel, Show writeModel) =>
                           Logger ->
                           GetCommandStream persistedStream ->
                           GetCommandTransactionStream persistedStream writeModel ->
                           Reading persistedStream ->
                           Writing persistedStream ->
                           CommandHandler writeModel ->
                           GetConsumeACommand persistedStream writeModel ->
                           ConsumeAnAggregate
getConsumeAnAggregate logger
                      getCommandStream
                      getCommandTransactionStream
                      reading @ Reading {querying}
                      writing
                      commandHandler
                      getConsumeACommandForAnAggregate =
  (\persistedAggregate @ PersistedItem {item = aggregateId} ->
    consumeCommandsOnAnAggregate
        logger
        getCommandStream
        getCommandTransactionStream
        reading
        (getConsumeACommandForAnAggregate
            logger
            querying
            writing
            getCommandTransactionStream
            commandHandler
            aggregateId)
        persistedAggregate)


consumeCommandsOnAnAggregate :: (FromJSON writeModel, Show writeModel) =>
                                                Logger ->
                                                GetCommandStream persistedStream ->
                                                GetCommandTransactionStream persistedStream writeModel ->
                                                Reading persistedStream ->
                                                ConsumeACommand ->
                                                ConsumeAnAggregate
consumeCommandsOnAnAggregate logger
                             getCommandStream
                             getCommandTransactionStream
                             reading @ Reading {querying}
                             consumeACommand
                             persistedAggregate @ PersistedItem { item = aggregateId} =  do
  threadId <- myThreadId
  logInfo logger $ "[consume.aggregate.commands] detected aggregrate "
    ++ (show $ item persistedAggregate)
    ++ "(thread "++ show threadId ++" locked on aggregate)"

  let commandStream = getCommandStream aggregateId
      commandTransactionStream = getCommandTransactionStream aggregateId

  try (StreamlySafe.runStreamOnIOAndThrowFailureTo
       threadId
       (consumeAnAggregateStream
             logger
             commandStream
             commandTransactionStream
             reading
             consumeACommand
             persistedAggregate))


consumeAnAggregateStream :: (FromJSON writeModel, Show writeModel) =>
                            Logger ->
                            CommandStream persistedStream ->
                            CommandTransactionStream persistedStream writeModel ->
                            Reading persistedStream ->
                            ConsumeACommand ->
                            ConsumeAnAggregateStream
consumeAnAggregateStream  logger
                      commandStream
                      commandTransactionStream
                      Reading { streaming = Streaming { streamFromOffset},
                                querying = Querying {retrieveLast},
                                subscribing }
                      consumeACommand
                      persistedAggregate @ PersistedItem {item = aggregateId} =
  (yieldAndSubscribeToAggregateUpdates subscribing commandStream persistedAggregate)
    & StreamlySafe.mapM (\PersistedItem {item = aggregateId}  -> do
        logInfo logger $ "[consume.aggregate.commands] consuming commands for aggregate " ++ (show aggregateId)
        response <- retrieveLast commandTransactionStream
        case response of
          Right lastCommandTransactionCall -> do
            let lastCommandOffsetConsumed = getCorrespondingPersistedCommandOffset lastCommandTransactionCall
            threadId <- myThreadId
            logInfo logger $ "[consume.aggregate.commands] last command offset consummed is   " ++ (show lastCommandOffsetConsumed)
            try (StreamlySafe.runStreamOnIOAndThrowFailureTo threadId
                  $ (streamFromOffset
                        commandStream $
                        fromMaybe 0 (fmap (+1) lastCommandOffsetConsumed))
                  & StreamlySafe.mapM (consumeACommand ))
          Left error -> return $ Left error)

getCorrespondingPersistedCommandOffset :: Maybe (Persisted (CommandTransaction writeModel) ) -> Maybe Offset
getCorrespondingPersistedCommandOffset (Just (PersistedItem { item = CommandTransaction { snapshot = Snapshot {offset}}})) = return offset
getCorrespondingPersistedCommandOffset (Nothing)= Nothing

yieldAndSubscribeToAggregateUpdates :: (Streamable stream monad Command, Streamable stream monad AggregateId) =>
                                       Subscribing persistedStream ->
                                       CommandStream persistedStream ->
                                       Persisted AggregateId ->
                                       stream monad (Either SomeException (Persisted AggregateId))
yieldAndSubscribeToAggregateUpdates Subscribing {subscribe}
                                    commandStream
                                    persistedAggregate @ PersistedItem { offset = offset , item = aggregateId} =
  (StreamlySafe.yield persistedAggregate)
    <> ((subscribe $ commandStream)
    & StreamlySafe.map (\newCommand -> PersistedItem { offset = offset,item = aggregateId }))
