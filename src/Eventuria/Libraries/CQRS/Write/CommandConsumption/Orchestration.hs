{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Eventuria.Libraries.CQRS.Write.CommandConsumption.Orchestration (getOrchestrationForAnAggregate)where

import           Control.Concurrent
import           Control.Exception

import           Data.Aeson
import           Data.Function ((&))

import           Eventuria.Commons.Logger.Core

import qualified Eventuria.Adapters.Streamly.Safe as StreamlySafe

import           Control.Monad.IO.Class (MonadIO(..))

import           Eventuria.Libraries.PersistedStreamEngine.Interface.Write.Writing
import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Read.Reading
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Streamable

import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import           Eventuria.Libraries.CQRS.Write.StreamRepository
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.Definitions

import           Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId

import           Eventuria.Libraries.CQRS.Write.Serialization.Command ()
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandTransaction
import           Eventuria.Libraries.CQRS.Write.Serialization.CommandTransaction ()

getOrchestrationForAnAggregate :: (FromJSON writeModel, Show writeModel) =>
                           Logger ->
                           GetCommandStream persistedStream ->
                           GetCommandTransactionStream persistedStream  ->
                           Reading persistedStream ->
                           Writing persistedStream ->
                           ProjectWriteModel writeModel ->
                           HandleCommand writeModel ->
                           OrchestratreCommandConsumptionForAggregate writeModel
getOrchestrationForAnAggregate logger
                      getCommandStream
                      getCommandTransactionStream
                      reading @ Reading {querying}
                      writing
                      projectWriteModel
                      commandHandler =
  (\persistedAggregate @ PersistedItem {item = aggregateId} -> do
    let commandStream = getCommandStream aggregateId
        commandTransactionStream = getCommandTransactionStream aggregateId
    orchestrate
        logger
        commandStream
        commandTransactionStream
        projectWriteModel
        reading
        writing
        commandHandler
        persistedAggregate)

{--
  The terminology is from CQRS Sagas, this section orchestrates the consumption of commands on a specific aggregate :
    - 1) Listening on commands arriving on the command stream
    - 2) Re-building the write model from the previous command transactions
    - 3) Handling the current persisted command
    - 4) Persisting the command transaction
 -}

orchestrate :: (FromJSON writeModel, Show writeModel) =>
                                                Logger ->
                                                CommandStream persistedStream ->
                                                CommandTransactionStream persistedStream  ->
                                                ProjectWriteModel writeModel ->
                                                Reading persistedStream ->
                                                Writing persistedStream ->
                                                HandleCommand writeModel ->
                                                OrchestratreCommandConsumptionForAggregate writeModel
orchestrate logger
            commandStream
            commandTransactionStream
            projectWriteModel
            Reading { streaming = Streaming { streamFromOffsetInclusive, streamFromRangeInclusive},
                                            querying = Querying {retrieveLast},
                                            subscribing }
            Writing {persist}
            handleCommand
            persistedAggregate @ PersistedItem { item = aggregateId} =  do

  threadId <- liftIO $ myThreadId
  liftIO $ logInfo logger $ "[consume.aggregate.commands] orchestrating aggregrate "
    ++ (show $ item persistedAggregate)
    ++ "(on thread "++ show threadId ++")"

  (listeningUpcomingCommands subscribing commandStream persistedAggregate)
    & StreamlySafe.mapM (\PersistedItem {item = aggregateId}  -> do
        logInfo logger $ "[consume.aggregate.commands] consuming commands for aggregate " ++ (show aggregateId)
        lastCommandTransactionStreamResult <- retrieveLast commandTransactionStream
        case lastCommandTransactionStreamResult of
          Right (Nothing) -> do
            logInfo logger $ "[consume.aggregate.commands] handling first command"
            handleAndPersistTransactionForDownstreamCommands projectWriteModel handleCommand Nothing 0
          Right (Just (PersistedItem { item = CommandTransaction {commandOffset,..}})) -> do

            logInfo logger $ "[consume.aggregate.commands] handling command with offset " ++ show commandOffset
            logInfo logger $ "[consume.aggregate.commands] reading events and constructing up to date write model."
            (rebuildWriteModelTillCommandOffset projectWriteModel commandOffset) >>=
              either
                (\error -> return $ Left error)
                (\writeModelMaybe -> do
                  logInfo logger $ "[consume.aggregate.commands] consumption with the following write model : " ++ show writeModelMaybe
                  handleAndPersistTransactionForDownstreamCommands
                    projectWriteModel
                    handleCommand
                    writeModelMaybe
                    (commandOffset +1))
          Left error -> return $ Left error)

  where
    rebuildWriteModelTillCommandOffset :: ProjectWriteModel writeModel -> Offset -> IO (Either SomeException (Maybe writeModel))
    rebuildWriteModelTillCommandOffset projectWriteModel commandOffset =
      StreamlySafe.foldx
        (\writeModelMaybe PersistedItem { item = commandTransaction @ CommandTransaction {commandHandlingResult}}  -> do
            projectWriteModel writeModelMaybe commandHandlingResult )
        Nothing
        id
        (streamFromRangeInclusive commandTransactionStream 0 commandOffset)

    handleAndPersistTransactionForDownstreamCommands :: ProjectWriteModel writeModel ->
                                                        HandleCommand writeModel ->
                                                        Maybe writeModel ->
                                                        Offset ->
                                                        IO (Either SomeException (Maybe writeModel))
    handleAndPersistTransactionForDownstreamCommands projectWriteModel
                                                     handleCommand
                                                     currentWriteModel
                                                     commandOffset  =
      StreamlySafe.foldxM
        (\writeModelMaybe persistedCommand -> do
            commandHandlingResponse <- handleCommand writeModelMaybe persistedCommand
            logInfo logger $ "[consume.command] persisting commandHandlingResponse : " ++ show commandHandlingResponse
            result <- persist commandTransactionStream $ toCommandTransaction
                                                  persistedCommand
                                                  commandHandlingResponse
            case result of
              Left error -> return $ Left error
              Right persistenceResult -> return $ Right $ projectWriteModel writeModelMaybe commandHandlingResponse)
        (return $ Right currentWriteModel)
        (\writeModelMaybe -> return $ Right writeModelMaybe)
        (streamFromOffsetInclusive commandStream $ commandOffset)

    listeningUpcomingCommands :: (Streamable stream monad Command, Streamable stream monad AggregateId) =>
                                           Subscribing persistedStream ->
                                           CommandStream persistedStream ->
                                           Persisted AggregateId ->
                                           stream monad (Either SomeException (Persisted AggregateId))
    listeningUpcomingCommands Subscribing {subscribe}
                              commandStream
                              persistedAggregate @ PersistedItem { offset = offset , item = aggregateId} =
      (StreamlySafe.yield persistedAggregate)
        <> ((subscribe $ commandStream)
        & StreamlySafe.map (\newCommand -> PersistedItem { offset = offset,item = aggregateId }))
