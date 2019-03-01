{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts      #-}
module Eventuria.Libraries.CQRS.Write.CommandConsumption.ConsumeAnAggregate (getConsumeAnAggregate)where

import           Control.Concurrent
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Exception

import           Data.Aeson
import           Data.Function ((&))
import           Data.Maybe

import           Eventuria.Commons.Logger.Core

import qualified Eventuria.Adapters.Streamly.Safe as StreamlySafe

import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Read.Reading
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Write.WDsl
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Streamable

import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import           Eventuria.Libraries.CQRS.Write.StreamRepository
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandHandler
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.Core
import           Eventuria.Libraries.CQRS.Write.Serialization.ValidationState ()
import           Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import           Eventuria.Libraries.CQRS.Write.Serialization.Command ()

getConsumeAnAggregate :: (FromJSON applicationState, Show applicationState) =>
                           Logger ->
                           GetCommandStream persistedStreamEngine ->
                           GetValidationStateStream persistedStreamEngine applicationState ->
                           Reading persistedStreamEngine ->
                           TransactionInterpreter applicationState () ->
                           CommandHandler applicationState ->
                           GetConsumeACommand persistedStreamEngine applicationState ->
                           ConsumeAnAggregate
getConsumeAnAggregate logger
                      getCommandStream
                      getValidationStateStream
                      reading @ Reading {querying}
                      transactionInterpreter
                      commandHandler
                      getConsumeACommandForAnAggregate =
  (\persistedAggregate @ PersistedItem {item = aggregateId} ->
    consumeCommandsOnAnAggregate
        logger
        getCommandStream
        getValidationStateStream
        reading
        (getConsumeACommandForAnAggregate
            logger
            querying
            getValidationStateStream
            transactionInterpreter
            commandHandler
            aggregateId)
        persistedAggregate)


consumeCommandsOnAnAggregate :: (FromJSON applicationState, Show applicationState) =>
                                                Logger ->
                                                GetCommandStream persistedStreamEngine ->
                                                GetValidationStateStream persistedStreamEngine applicationState ->
                                                Reading persistedStreamEngine ->
                                                ConsumeACommand ->
                                                ConsumeAnAggregate
consumeCommandsOnAnAggregate logger
                             getCommandStream
                             getValidationStateStream
                             reading @ Reading {querying}
                             consumeACommand
                             persistedAggregate @ PersistedItem { item = aggregateId} =  do
  threadId <- myThreadId
  liftIO $ logInfo logger $ "detected aggregrate " ++ (show $ item persistedAggregate)
    ++ " > Thread " ++ show threadId ++ " is locked for this aggregate"
  let commandStream = getCommandStream aggregateId
      validationStateStream = getValidationStateStream aggregateId

  try (StreamlySafe.runStreamOnIOAndThrowFailureTo
       threadId
       (consumeAnAggregateStream
             logger
             commandStream
             validationStateStream
             reading
             consumeACommand
             persistedAggregate))


consumeAnAggregateStream :: (FromJSON applicationState, Show applicationState) =>
                                          Logger ->
                                          CommandStream persistedStreamEngine ->
                                          ValidateStateStream persistedStreamEngine applicationState ->
                                          Reading persistedStreamEngine ->
                                          ConsumeACommand ->
                                          ConsumeAnAggregateStream
consumeAnAggregateStream  logger
                      commandStream
                      validationStateStream
                      Reading { streaming = Streaming { streamFromOffset},
                                querying = Querying {retrieveLast},
                                subscribing }
                      consumeACommand
                      persistedAggregate @ PersistedItem {item = aggregateId} =
  (yieldAndSubscribeToAggregateUpdates subscribing commandStream persistedAggregate)
    & StreamlySafe.mapM (\PersistedItem {item = aggregateId}  -> do
        liftIO $ logInfo logger $ "processing commands for aggregate " ++ (show aggregateId)
        response <- liftIO $ retrieveLast validationStateStream
        case response of
          Right lastValidationStateCall -> do
            let lastOffsetConsumed = getLastOffsetConsumed lastValidationStateCall
            threadId <- myThreadId
            liftIO $ logInfo logger $ "last offset command consummed is   " ++ (show lastOffsetConsumed)
            try (StreamlySafe.runStreamOnIOAndThrowFailureTo threadId
                  $ (streamFromOffset
                        commandStream $
                        fromMaybe 0 (fmap (+1) lastOffsetConsumed))
                  & StreamlySafe.mapM (consumeACommand ))
          Left error -> return $ Left error)

getLastOffsetConsumed :: Maybe (Persisted (ValidationState applicationState)) ->
                         Maybe Offset
getLastOffsetConsumed lastValidationStateCall =
  fmap ( \persistedValidationState ->
    lastOffsetConsumed $ item $ persistedValidationState ) $ lastValidationStateCall

yieldAndSubscribeToAggregateUpdates :: (Streamable stream monad Command, Streamable stream monad AggregateId) =>
                                       Subscribing persistedStreamEngine ->
                                       CommandStream persistedStreamEngine ->
                                       Persisted AggregateId ->
                                       stream monad (Either SomeException (Persisted AggregateId))
yieldAndSubscribeToAggregateUpdates Subscribing {subscribe}
                                    commandStream
                                    persistedAggregate @ PersistedItem { offset = offset , item = aggregateId} =
  (StreamlySafe.yield persistedAggregate)
    <> ((subscribe $ commandStream)
    & StreamlySafe.map (\newCommand -> PersistedItem { offset = offset,item = aggregateId }))
