{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE Rank2Types #-}
module Cqrs.CommandConsumerFlow  where

import Cqrs.Aggregate.Commands.Command


import Streamly
import qualified Streamly.Prelude as S
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function ((&))

import Cqrs.Logger
import Data.Maybe

import Cqrs.Aggregate.Commands.ValidationStates.ValidationStateStream
import Cqrs.Aggregate.Commands.CommandStream
import Cqrs.Aggregate.Ids.AggregateIdStream
import Cqrs.CommandHandler
import Cqrs.EventStore.Interpreter
import Cqrs.EventStore.Translation
import Cqrs.EDsl
import Cqrs.EventStore.EDsl
import Cqrs.EventStore.Streaming
import Cqrs.EventStore.Querying
import Cqrs.EventStore.Context
import Cqrs.EventStore.PersistedItem

runCommandConsumers :: Logger -> EventStoreContext -> AggregateIdStream  -> CommandHandler -> IO ()
runCommandConsumers logger eventStoreContext @ Context { credentials = credentials, connection = connection } aggregateStream commandHandler = do
  logInfo logger "starting streams"
  runStream
    $ parallely
    $ streamAllInfinitely aggregateStream
    & S.mapM (\persistedAggregate -> do
      runStream
        $ serially
        $ yieldAndSubscribeToAggregateUpdates eventStoreContext persistedAggregate
        & S.mapM (\PersistedItem {item = aggregateId}  -> do
            liftIO $ logInfo logger $ "processing commands workspace for " ++ (show aggregateId)
            let validationStateStream = getValidateStateStream eventStoreContext aggregateId
            let commandStream = getCommandStream eventStoreContext aggregateId
            lastOffsetConsumed <- liftIO $ retrieveLastOffsetConsumed validationStateStream
            runStream
              $ (streamFromOffset commandStream $ fromMaybe 0 lastOffsetConsumed)
              & S.mapM (\persistedCommand @PersistedItem { item = Command { commandHeader = CommandHeader {commandId = commandId} }}  -> do
                lastValidationState <- liftIO $ (fmap.fmap) item $ retrieveLast validationStateStream
                let transaction = case (commandHandler persistedCommand lastValidationState) of
                                    Reject reason -> rejectCommandTransaction lastValidationState aggregateId commandId reason
                                    SkipBecauseAlreadyProcessed -> skipCommandTransaction aggregateId commandId
                                    Transact commandTransaction -> (translate $ commandTransaction)
                interpret transaction eventStoreContext)))