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
import Cqrs.PersistedStreamEngine.Write.CqrsEDslToWDslTranslation
import Cqrs.EDsl
import Cqrs.PersistedStreamEngine.Write.WDsl
import Cqrs.PersistedStreamEngine.Repository
import Cqrs.PersistedStreamEngine.Offset
import Cqrs.Aggregate.Commands.ValidationStates.ValidationState
import Cqrs.Aggregate.Ids.AggregateId
import Cqrs.PersistedStreamEngine.Read.Interface
import Cqrs.PersistedStreamEngine.PersistedItem

import Cqrs.Serialization.Aggregate.Command ()
import Cqrs.Serialization.Aggregate.ValidationState ()


runCommandConsumers :: Logger -> CqrsStreamRepository persistedStream -> Reading persistedStream -> CommandHandler -> InterpreterWritePersistedStreamLanguage persistedStream () -> IO ()
runCommandConsumers logger
                    streamRepository @ CqrsStreamRepository { aggregateIdStream, getCommandStream, getValidationStateStream }
                    Reading { streaming = Streaming {streamAllInfinitely, streamFromOffset},
                              querying = querying @ Querying {retrieveLast},
                              subscribing }
                    commandHandler interpreterWritePersistedStreamLanguage = do
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
                interpreterWritePersistedStreamLanguage transaction logger streamRepository)))


getLastOffsetConsumed :: IO( Maybe (Persisted ValidationState)) -> IO (Maybe Offset)
getLastOffsetConsumed lastValidationStateCall = (fmap.fmap) ( \persistedValidationState -> lastOffsetConsumed $ item $ persistedValidationState ) $ lastValidationStateCall

yieldAndSubscribeToAggregateUpdates :: (Streamable stream monad Command, Streamable stream monad AggregateId) =>
                                       Subscribing persistedStream -> GetCommandStream persistedStream ->
                                       Persisted AggregateId ->
                                       stream monad (Persisted AggregateId)
yieldAndSubscribeToAggregateUpdates Subscribing {subscribe} getCommandStream persistedAggregate @ PersistedItem { offset = offset , item = aggregateId} =
  (S.yield persistedAggregate) <> ((subscribe $ getCommandStream aggregateId) & S.map (\newCommand -> PersistedItem { offset = offset,item = aggregateId }))



