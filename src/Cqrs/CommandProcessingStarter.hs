{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE Rank2Types #-}
module Cqrs.CommandProcessingStarter  where

import Cqrs.Aggregate.Commands.Command
import qualified Cqrs.Aggregate.Commands.CommandStream as CommandStream

import Cqrs.Aggregate.Commands.ValidationStates.ValidationStateStream
import Cqrs.Aggregate.Commands.ValidationStates.PersistedValidationState
import Streamly
import qualified Streamly.Prelude as S
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function ((&))

import Cqrs.Logger


import Cqrs.Aggregate.Commands.PersistedCommand
import Cqrs.Aggregate.Ids.AggregateIdStream
import Cqrs.CommandHandler
import Cqrs.EventStore.Interpreter
import Cqrs.EventStore.Translation
import Cqrs.EDsl
import Cqrs.EventStore.EDsl
import Cqrs.Aggregate.Ids.PersistedAggregateId
import Cqrs.EventStore.Streaming

import Cqrs.EventStore.Querying
import Cqrs.EventStore.Context

startProcessingCommands :: Logger -> EventStoreContext -> AggregateStream  -> CommandHandler -> IO ()
startProcessingCommands logger eventStoreContext @ Context { credentials = credentials, connection = connection } aggregateStream commandHandler = do
  logInfo logger "starting streams"
  runStream
    $ parallely
    $ streamAllInfinitely aggregateStream
    & S.mapM (\persistedAggregate -> do
      liftIO $ logInfo logger $ "detected workspace id : " ++ (show persistedAggregate)
      runStream
        $ serially
        $ yieldAndSubscribeToAggregateUpdates eventStoreContext persistedAggregate
        & S.mapM (\PersistedAggregateId {persistedAggregateId = aggregateId}  -> do
            liftIO $ logInfo logger $ "processing commands workspace for " ++ (show aggregateId)
            let validationStateStream = getValidateStateStream eventStoreContext aggregateId
            lastOffsetConsumed <- liftIO $ retrieveLastOffsetConsumed validationStateStream
            runStream
              $ (CommandStream.readForward credentials connection aggregateId lastOffsetConsumed)
              & S.mapM (\persistedCommand @PersistedCommand { command = Command { commandHeader = CommandHeader {commandId = commandId} }}  -> do
                lastValidationState <- liftIO $ (fmap.fmap) validationState $ retrieveLast validationStateStream
                let transaction = case (commandHandler persistedCommand lastValidationState) of
                                    Reject reason -> rejectCommandTransaction lastValidationState aggregateId commandId reason
                                    SkipBecauseAlreadyProcessed -> skipCommandTransaction aggregateId commandId
                                    Transact commandTransaction -> (translate $ commandTransaction)
                interpret transaction eventStoreContext)))