{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NamedFieldPuns #-}
module Cqrs.CommandConsumerFlow  where

import Cqrs.Aggregate.Commands.Command


import Streamly
import qualified Streamly.Prelude as S
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function ((&))

import Logger.Core
import Data.Maybe

import Cqrs.CommandHandler
import Plugins.EventStore.InterpreterEventStore
import Cqrs.EventStore.Translation
import Cqrs.EDsl
import Cqrs.EventStore.Write.WDsl
import EventStore.Read.Streaming
import EventStore.Read.Querying
import EventStore.Read.PersistedItem
import Control.Concurrent
import Cqrs.Aggregate.StreamRepository
import Cqrs.Streams
import Cqrs.Aggregate.Commands.ValidationStates.ValidationState
import EventStore.Streamable
import Cqrs.Aggregate.Ids.AggregateId
import EventStore.Read.Subscribing

runCommandConsumers :: Logger -> EventStoreStreamRepository  -> CommandHandler -> IO ()
runCommandConsumers logger streamRepository @ StreamRepository { aggregateIdStream, getCommandStream, getValidationStateStream } commandHandler = do
  logInfo logger "runnning command consummers"
  runStream
    $ parallely
    $ streamAllInfinitely aggregateIdStream
    & S.mapM (\persistedAggregate -> do
      threadId <- myThreadId
      liftIO $ logInfo logger $ "detected aggregrate " ++ (show $ item persistedAggregate) ++ " > Thread " ++ show threadId ++ " is locked for this aggregate"
      runStream
        $ serially
        $ yieldAndSubscribeToAggregateUpdates getCommandStream persistedAggregate
        & S.mapM (\PersistedItem {item = aggregateId}  -> do
            liftIO $ logInfo logger $ "processing commands for aggregate " ++ (show aggregateId)
            let validationStateStream = getValidationStateStream aggregateId
            let commandStream = getCommandStream aggregateId
            lastOffsetConsumed <- liftIO $ retrieveLastOffsetConsumed validationStateStream
            liftIO $ logInfo logger $ "last offset command consummed is   " ++ (show lastOffsetConsumed)
            runStream
              $ serially
              $ (streamFromOffset commandStream $ fromMaybe 0 (fmap (+1) lastOffsetConsumed))
              & S.mapM (\persistedCommand @PersistedItem { item = Command { commandHeader = CommandHeader {commandId = commandId} }}  -> do
                lastValidationState <- liftIO $ (fmap.fmap) item $ retrieveLast validationStateStream
                liftIO $ logInfo logger $ "feeding command handler > " ++ (show persistedCommand) ++ " > validationState : " ++ show lastValidationState
                let transaction = case (commandHandler persistedCommand lastValidationState) of
                                    Reject reason -> rejectCommandTransaction lastValidationState aggregateId commandId reason
                                    SkipBecauseAlreadyProcessed -> skipCommandTransaction aggregateId commandId
                                    Validate commandTransaction -> validateCommandTransaction aggregateId commandId (translate $ commandTransaction)
                interpretWriteEventStoreLanguage transaction logger streamRepository)))


retrieveLastOffsetConsumed :: ValidateStateStream -> IO (Maybe Offset)
retrieveLastOffsetConsumed validationStateStream = (fmap.fmap) ( \persistedValidationState -> lastOffsetConsumed $ item $ persistedValidationState ) (retrieveLast validationStateStream)

yieldAndSubscribeToAggregateUpdates :: (Streamable monad stream Command, Streamable monad stream AggregateId) =>
                                       GetCommandStream ->
                                       Persisted AggregateId ->
                                       stream monad (Persisted AggregateId)
yieldAndSubscribeToAggregateUpdates getCommandStream persistedAggregate @ PersistedItem { offset = offset , item = aggregateId} =
  (S.yield persistedAggregate) <>
  ((subscribeToAggregateUpdates getCommandStream aggregateId  ) & S.map (\aggregateId -> PersistedItem { offset = offset,item = aggregateId }))


subscribeToAggregateUpdates :: (Streamable monad stream Command, Streamable monad stream AggregateId)  =>
                                  GetCommandStream -> AggregateId ->
                                  stream monad AggregateId
subscribeToAggregateUpdates getCommandStream aggregateId =
  (subscribe $ getCommandStream aggregateId) & S.map (\persistedCommand -> aggregateId)