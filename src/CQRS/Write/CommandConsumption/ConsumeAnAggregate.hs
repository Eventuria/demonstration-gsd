{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts      #-}
module CQRS.Write.CommandConsumption.ConsumeAnAggregate (getConsumeAnAggregate)where

import Data.Aeson
import Logger.Core
import PersistedStreamEngine.Interface.PersistedItem
import CQRS.Write.Aggregate.Commands.Command
import CQRS.Write.StreamRepository
import PersistedStreamEngine.Interface.Read.Reading
import PersistedStreamEngine.Interface.Write.WDsl
import CQRS.Write.CommandConsumption.CommandHandler
import CQRS.Write.CommandConsumption.Core
import Control.Monad.IO.Class (MonadIO(..))
import CQRS.Write.Serialization.ValidationState ()
import CQRS.Write.Aggregate.Ids.AggregateId
import qualified Streamly.Safe as StreamlySafe
import Control.Concurrent
import Control.Exception
import PersistedStreamEngine.Interface.Offset
import CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import PersistedStreamEngine.Interface.Streamable
import System.SafeResponse
import Data.Function ((&))
import Data.Maybe
import CQRS.Write.Serialization.Command ()

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
                                       stream monad (SafeResponse (Persisted AggregateId))
yieldAndSubscribeToAggregateUpdates Subscribing {subscribe}
                                    commandStream
                                    persistedAggregate @ PersistedItem { offset = offset , item = aggregateId} =
  (StreamlySafe.yield persistedAggregate)
    <> ((subscribe $ commandStream)
    & StreamlySafe.map (\newCommand -> PersistedItem { offset = offset,item = aggregateId }))
