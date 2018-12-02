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
import EventStore.Read.PersistedItem
import Control.Concurrent
import Cqrs.Aggregate.StreamRepository
import Cqrs.Streams
import Cqrs.Aggregate.Commands.ValidationStates.ValidationState
import EventStore.Streamable
import Cqrs.Aggregate.Ids.AggregateId
import EventStore.EventStore

runCommandConsumers :: Logger -> EventStoreStreamRepository  -> EventStoreReading -> CommandHandler -> IO ()
runCommandConsumers logger
                    streamRepository @ StreamRepository { aggregateIdStream, getCommandStream, getValidationStateStream }
                    Reading { streaming = Streaming {streamAllInfinitely, streamFromOffset},
                              querying = querying @ Querying {retrieveLast},
                              subscribing }
                    commandHandler = do
  logInfo logger "runnning command consummers"
  runStream
    $ parallely
    $ streamAllInfinitely aggregateIdStream
    & S.mapM (\persistedAggregate -> do
      threadId <- myThreadId
      liftIO $ logInfo logger $ "detected aggregrate " ++ (show $ item persistedAggregate) ++ " > Thread " ++ show threadId ++ " is locked for this aggregate"
      runStream
        $ serially
        $ yieldAndSubscribeToAggregateUpdates subscribing getCommandStream persistedAggregate
        & S.mapM (\PersistedItem {item = aggregateId}  -> do
            liftIO $ logInfo logger $ "processing commands for aggregate " ++ (show aggregateId)
            let validationStateStream = getValidationStateStream aggregateId
            let commandStream = getCommandStream aggregateId
            lastOffsetConsumed <- liftIO $ retrieveLastOffsetConsumed querying validationStateStream
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


retrieveLastOffsetConsumed :: EventStoreQuerying -> ValidateStateStream -> IO (Maybe Offset)
retrieveLastOffsetConsumed Querying {retrieveLast} validationStateStream = (fmap.fmap) ( \persistedValidationState -> lastOffsetConsumed $ item $ persistedValidationState ) (retrieveLast validationStateStream)

yieldAndSubscribeToAggregateUpdates :: (Streamable monad stream Command, Streamable monad stream AggregateId) =>
                                       EventStoreSubscribing -> GetCommandStream ->
                                       Persisted AggregateId ->
                                       stream monad (Persisted AggregateId)
yieldAndSubscribeToAggregateUpdates subscribing getCommandStream persistedAggregate @ PersistedItem { offset = offset , item = aggregateId} =
  (S.yield persistedAggregate) <>
  ((subscribeToAggregateUpdates subscribing getCommandStream aggregateId  ) & S.map (\aggregateId -> PersistedItem { offset = offset,item = aggregateId }))


subscribeToAggregateUpdates :: (Streamable monad stream Command, Streamable monad stream AggregateId)  =>
                                  EventStoreSubscribing -> GetCommandStream -> AggregateId ->
                                  stream monad AggregateId
subscribeToAggregateUpdates Subscribing {subscribe} getCommandStream aggregateId =
  (subscribe $ getCommandStream aggregateId) & S.map (\persistedCommand -> aggregateId)