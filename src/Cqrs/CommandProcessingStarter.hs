{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE Rank2Types #-}
module Cqrs.CommandProcessingStarter  where

import Cqrs.Command
import qualified Cqrs.Command as CommandModule
import qualified Cqrs.CommandResponse as CommandResponse
import Cqrs.Events
import Cqrs.Snapshot

import Data.Maybe
import Cqrs.CommandStream


import qualified Cqrs.AggregateStream as AggregateStream
import qualified Cqrs.CommandStream as CommandStream

import qualified Cqrs.SnapshotStream as SnapshotStream
import Control.Concurrent.Async (wait)
import Data.Semigroup (Semigroup(..))


import Streamly
import qualified Streamly.Prelude as S
import Control.Concurrent
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function ((&))

import qualified Database.EventStore as EventStore
import Control.Exception
import Cqrs.Logger
import Cqrs.Core
import Cqrs.Streams
import Cqrs.PersistedCommand
import Cqrs.AggregateStream
import Cqrs.CommandHandler
import Cqrs.EventStore.Interpreter
import Cqrs.EventStore.Translation
import Cqrs.EDsl
import Cqrs.EventStore.EDsl

startProcessingCommands :: Logger -> EventStore.Connection -> CommandHandler -> IO ()
startProcessingCommands logger eventStoreConnection commandHandler = do
  logInfo logger "starting streams"
  runStream
    $ parallely
    $ (AggregateStream.streamAllInfinitely logger eventStoreConnection)
    & S.mapM (\persistedWorkspace -> do
      liftIO $ logInfo logger $ "detected workspace id : " ++ (show persistedWorkspace)
      runStream
        $ serially
        $ yieldAndSubscribeToAggregateUpdates logger eventStoreConnection persistedWorkspace
        & S.mapM (\PersistedAggregate {aggregateIdPersisted = aggregateId}  -> do
            liftIO $ logInfo logger $ "processing commands workspace for " ++ (show aggregateId)
            lastOffsetConsumed <- liftIO $ SnapshotStream.retrieveLastOffsetConsumed eventStoreConnection aggregateId
            runStream
              $ (CommandStream.readForward eventStoreConnection aggregateId lastOffsetConsumed)
              & S.mapM (\persistedCommand @PersistedCommand { command = Command { commandHeader = CommandHeader {commandId = commandId} }}  -> do
                lastSnapshot <- liftIO $ SnapshotStream.retrieveLast eventStoreConnection aggregateId
                let transaction = case (commandHandler persistedCommand lastSnapshot) of
                                    Reject reason -> rejectCommandTransaction lastSnapshot aggregateId commandId reason
                                    SkipBecauseAlreadyProcessed -> skipCommandTransaction aggregateId commandId
                                    Transact commandTransaction -> (translate $ commandTransaction)
                interpret transaction logger eventStoreConnection)))