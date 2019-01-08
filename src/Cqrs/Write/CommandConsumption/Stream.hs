{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NamedFieldPuns #-}
module Cqrs.Write.CommandConsumption.Stream  where

import Streamly
import qualified Streamly.Prelude as S
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function ((&))
import Control.Concurrent
import Data.Maybe
import Data.Aeson
import PersistedStreamEngine.Interface.Streamable

import Logger.Core

import Cqrs.Write.Aggregate.Commands.Command
import Cqrs.Write.CommandConsumption.CommandHandler
import PersistedStreamEngine.Interface.Write.CqrsEDslToWDslTranslation
import Cqrs.EDsl
import PersistedStreamEngine.Interface.Write.WDsl
import Cqrs.Write.StreamRepository
import PersistedStreamEngine.Interface.Offset
import Cqrs.Write.Aggregate.Commands.ValidationStates.ValidationState
import Cqrs.Write.Aggregate.Ids.AggregateId
import PersistedStreamEngine.Interface.Read.Reading
import PersistedStreamEngine.Interface.PersistedItem
import Cqrs.Write.Aggregate.Commands.CommandHeader
import Cqrs.Write.Serialization.Command ()
import Cqrs.Write.Serialization.ValidationState ()


stream :: (FromJSON applicationState, Show applicationState) => Logger -> CqrsStreamRepository persistedStream applicationState -> Reading persistedStream -> CommandHandler applicationState -> InterpreterWritePersistedStreamLanguage persistedStream applicationState () -> IO ()
stream logger
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
              & S.mapM (\persistedCommand @PersistedItem { item = Command { commandHeader = commandHeader@CommandHeader {commandId} }}  -> do
                lastValidationState <- liftIO $ (fmap.fmap) item $ retrieveLast validationStateStream
                liftIO $ logInfo logger $ "feeding command handler > " ++ (show persistedCommand) ++ " > validationState : " ++ show lastValidationState
                let transaction = case (commandHandler persistedCommand lastValidationState) of
                                    Reject reason -> rejectCommandTransaction lastValidationState commandHeader reason
                                    SkipBecauseAlreadyProcessed -> skipCommandTransaction commandHeader
                                    Validate commandTransaction -> validateCommandTransaction commandHeader (translate $ commandTransaction)
                interpreterWritePersistedStreamLanguage transaction logger streamRepository)))


getLastOffsetConsumed :: IO( Maybe (Persisted (ValidationState applicationState))) -> IO (Maybe Offset)
getLastOffsetConsumed lastValidationStateCall = (fmap.fmap) ( \persistedValidationState -> lastOffsetConsumed $ item $ persistedValidationState ) $ lastValidationStateCall

yieldAndSubscribeToAggregateUpdates :: (Streamable stream monad Command, Streamable stream monad AggregateId) =>
                                       Subscribing persistedStream -> GetCommandStream persistedStream ->
                                       Persisted AggregateId ->
                                       stream monad (Persisted AggregateId)
yieldAndSubscribeToAggregateUpdates Subscribing {subscribe} getCommandStream persistedAggregate @ PersistedItem { offset = offset , item = aggregateId} =
  (S.yield persistedAggregate) <> ((subscribe $ getCommandStream aggregateId) & S.map (\newCommand -> PersistedItem { offset = offset,item = aggregateId }))



