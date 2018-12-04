{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NamedFieldPuns #-}
module Cqrs.CommandConsumerFlow  where

import Streamly
import qualified Streamly.Prelude as S
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function ((&))
import Control.Concurrent
import Data.Maybe

import Streamly.Streamable

import Logger.Core

import Cqrs.Aggregate.Commands.Command
import Cqrs.CommandHandler
import Cqrs.PersistedStream.Translation
import Cqrs.EDsl
import Cqrs.PersistedStream.Write.WDsl
import Cqrs.Aggregate.StreamRepository
import Cqrs.Streams
import Cqrs.Aggregate.Commands.ValidationStates.ValidationState
import Cqrs.Aggregate.Ids.AggregateId
import qualified Cqrs.PersistedStream.Stream as PersistedStream
import Cqrs.PersistedStream.Stream
import Cqrs.PersistedStream.PersistedItem

-- to be removed
import Plugins.GregYoungEventStore.InterpreterEventStore


runCommandConsumers :: Logger -> EventStoreStreamRepository  -> PersistedStream.Reading -> CommandHandler -> InterpreterWriteEventStoreLanguage () -> IO ()
runCommandConsumers logger
                    streamRepository @ StreamRepository { aggregateIdStream, getCommandStream, getValidationStateStream }
                    Reading { streaming = Streaming {streamAllInfinitely, streamFromOffset},
                              querying = querying @ Querying {retrieveLast},
                              subscribing }
                    commandHandler interpretWriteEventStoreLanguage = do
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
            lastOffsetConsumed <- liftIO $ getLastOffsetConsumed (retrieveLast validationStateStream)
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


getLastOffsetConsumed :: IO( Maybe (Persisted ValidationState)) -> IO (Maybe Offset)
getLastOffsetConsumed lastValidationStateCall = (fmap.fmap) ( \persistedValidationState -> lastOffsetConsumed $ item $ persistedValidationState ) $ lastValidationStateCall

yieldAndSubscribeToAggregateUpdates :: (Streamable stream monad Command, Streamable stream monad AggregateId) =>
                                       PersistedStream.Subscribing -> GetCommandStream ->
                                       Persisted AggregateId ->
                                       stream monad (Persisted AggregateId)
yieldAndSubscribeToAggregateUpdates Subscribing {subscribe} getCommandStream persistedAggregate @ PersistedItem { offset = offset , item = aggregateId} =
  (S.yield persistedAggregate) <> ((subscribe $ getCommandStream aggregateId) & S.map (\newCommand -> PersistedItem { offset = offset,item = aggregateId }))



