{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.TransactionInterpreter where

import           Control.Monad.Free
import           Control.Exception
import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.Time as Time
import qualified Data.UUID.V4 as Uuid
import           Data.Aeson
import           Data.Either

import           Eventuria.Commons.Logger.Core
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Write.TransactionDSL
import           Eventuria.Libraries.CQRS.Write.Aggregate.Events.Event
import           Eventuria.Libraries.CQRS.Write.StreamRepository

import           Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.EventStoreStream
import           Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Write.Transactions
import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem

import           Eventuria.Libraries.CQRS.Write.Serialization.Command ()
import           Eventuria.Libraries.CQRS.Write.Serialization.AggregateId ()
import           Eventuria.Libraries.CQRS.Write.Serialization.Event ()
import           Eventuria.Libraries.CQRS.Write.Serialization.ValidationState ()
import           Eventuria.Libraries.CQRS.Write.Serialization.CommandResponse ()

import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandHeader
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState


interpretTransactionOverEventStore :: (ToJSON applicationState , Show applicationState) =>
                                        Logger ->
                                        CQRSStreamRepository EventStoreStream applicationState ->
                                        Persisted Command ->
                                        Transaction applicationState a ->
                                        IO (Either SomeException a)
interpretTransactionOverEventStore logger
                          cqrsStreamRepository @ CQRSStreamRepository {getEventStream,
                                                                       getValidationStateStream,getCommandResponseStream}
                          PersistedItem {item = command @ Command {commandHeader = CommandHeader {aggregateId}}}
                          transaction  =
   (logInfo logger  $ "[command.transaction] start : " ++ show command) >>
   (startTransactions
       (getEventStream           aggregateId)
       (getValidationStateStream aggregateId)
       (getCommandResponseStream aggregateId) ) >>=
   either
    (\error -> return $ Left error)
    (\(eventStream,validationStateStream,commandResponseStream) ->
          interpret
            logger
            eventStream
            validationStateStream
            commandResponseStream
            transaction >>=
          either
            (\error -> return $ Left error)
            (\transactionEnd -> do
                result <- endTransactions
                            eventStream
                            validationStateStream
                            commandResponseStream
                return $ (\_ -> transactionEnd)  <$> result ))

  where
    endTransactions :: EventStoreStreamUnderTransaction Event ->
                       EventStoreStreamUnderTransaction (ValidationState applicationState) ->
                       EventStoreStreamUnderTransaction CommandResponse -> IO (Either SomeException ())
    endTransactions eventStream validationStateStream commandResponseStream = do
        eventStreamResult           <- endTransaction eventStream
        validationStateStreamResult <- endTransaction validationStateStream
        commandResponseStreamResult <- endTransaction commandResponseStream
        case (partitionEithers [eventStreamResult,validationStateStreamResult,commandResponseStreamResult]) of
         (transactionAborted,commits) | length transactionAborted == 0 -> do
            logInfo logger  $ "[command.transaction] end successfully : " ++ show command
            return $ Right ()
         (transactionAborted,commits) -> do
            logInfo logger  $ "[command.transaction] transaction failed causing an inconsistency in the system : " ++ show command
            logInfo logger  $ "[command.transaction] commit " ++ (show.length) commits ++ "/" ++ (show.length) transactionAborted
            return $ Left $ head transactionAborted


    startTransactions :: EventStoreStream Event ->
                         EventStoreStream (ValidationState applicationState) ->
                         EventStoreStream CommandResponse ->
                         IO (Either SomeException (EventStoreStreamUnderTransaction Event ,
                                                   EventStoreStreamUnderTransaction (ValidationState applicationState) ,
                                                   EventStoreStreamUnderTransaction CommandResponse))
    startTransactions eventStream validationStateStream commandResponseStream = do
      eventStreamResult           <- startTransaction eventStream
      validationStateStreamResult <- startTransaction validationStateStream
      commandResponseStreamResult <- startTransaction commandResponseStream
      return $ groupResults
                  eventStreamResult
                  validationStateStreamResult
                  commandResponseStreamResult

       where
        groupResults :: Either SomeException (EventStoreStreamUnderTransaction Event) ->
                      Either SomeException (EventStoreStreamUnderTransaction (ValidationState applicationState) ) ->
                      Either SomeException (EventStoreStreamUnderTransaction CommandResponse) ->
                      Either SomeException (EventStoreStreamUnderTransaction Event ,
                                            EventStoreStreamUnderTransaction (ValidationState applicationState) ,
                                            EventStoreStreamUnderTransaction CommandResponse)
        groupResults eventStreamResult validationStateStreamResult commandResponseStreamResult = do
            eventStream <- eventStreamResult
            validationStateStream <- validationStateStreamResult
            commandResponseStream <- commandResponseStreamResult
            return (eventStream,validationStateStream,commandResponseStream)


interpret :: (ToJSON applicationState , Show applicationState) =>
              Logger ->
              EventStoreStreamUnderTransaction Event ->
              EventStoreStreamUnderTransaction (ValidationState applicationState) ->
              EventStoreStreamUnderTransaction CommandResponse ->
              Transaction applicationState a ->
              IO (Either SomeException a)

interpret logger eventStream validationStateStream commandResponseStream  (Pure a) = return $ Right a

interpret logger eventStream validationStateStream commandResponseStream (Free (PersistEvent event nextTransactionChunk))  = do
    liftIO $ logInfo logger  $ "[command.transaction] persist event : " ++ (show event)
    persist eventStream event
    interpret logger eventStream validationStateStream commandResponseStream nextTransactionChunk

interpret logger eventStream validationStateStream commandResponseStream (Free (PersistValidationState validationState nextTransactionChunk)) = do
    liftIO $ logInfo logger $ "[command.transaction] persist validationState : " ++ (show validationState)
    persist validationStateStream validationState
    interpret logger eventStream validationStateStream commandResponseStream nextTransactionChunk

interpret logger eventStream validationStateStream commandResponseStream (Free (PersistCommandResponse commandResponse nextTransactionChunk))  = do
    liftIO $ logInfo logger $ "[command.transaction] persist response : " ++ (show commandResponse)
    persist commandResponseStream commandResponse
    interpret logger eventStream validationStateStream commandResponseStream nextTransactionChunk

interpret logger eventStream validationStateStream commandResponseStream (Free (GetCurrentTime fct))  = do
    now <- Time.getCurrentTime
    interpret logger eventStream validationStateStream commandResponseStream (fct now)

interpret logger eventStream validationStateStream commandResponseStream  (Free (GetNewEventId fct))  = do
    eventId <- liftIO $ Uuid.nextRandom
    interpret logger eventStream validationStateStream commandResponseStream (fct eventId)