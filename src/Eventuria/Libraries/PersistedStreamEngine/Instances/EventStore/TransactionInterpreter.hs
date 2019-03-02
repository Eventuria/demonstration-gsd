module Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.TransactionInterpreter where

import Control.Monad.Free
import qualified Data.Time as Time
import qualified Data.UUID.V4 as Uuid
import Control.Monad.IO.Class (MonadIO(..))

import Eventuria.Commons.Logger.Core
import Data.Aeson
import Eventuria.Libraries.PersistedStreamEngine.Interface.Write.TransactionDSL
import Eventuria.Libraries.CQRS.Write.Aggregate.Events.Event
import Eventuria.Libraries.CQRS.Write.Aggregate.Core
import Eventuria.Libraries.CQRS.Write.StreamRepository

import Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Write.Persisting
import Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.EventStoreStream

import Eventuria.Libraries.CQRS.Write.Serialization.Command ()
import Eventuria.Libraries.CQRS.Write.Serialization.AggregateId ()
import Eventuria.Libraries.CQRS.Write.Serialization.Event ()
import Eventuria.Libraries.CQRS.Write.Serialization.ValidationState ()
import Eventuria.Libraries.CQRS.Write.Serialization.CommandResponse ()



transactionInterpreterForEventStore :: (ToJSON applicationState , Show applicationState) =>
                                        Logger ->
                                        CqrsStreamRepository EventStoreStream applicationState ->
                                        TransactionInterpreter applicationState a

transactionInterpreterForEventStore logger streamRepository (Pure a) = return $ Right a

transactionInterpreterForEventStore logger streamRepository (Free (TransactionStart command next))  = do
    liftIO $ logInfo logger  $ "[command.transaction] start : " ++ show command
    transactionInterpreterForEventStore logger streamRepository next

transactionInterpreterForEventStore logger streamRepository (Free (TransactionEnd command next))  = do
    liftIO $ logInfo logger  $ "[command.transaction] end : " ++ show command
    transactionInterpreterForEventStore logger streamRepository next

transactionInterpreterForEventStore logger streamRepository (Free (PersistEvent event next))  = do
    liftIO $ logInfo logger  $ "[command.transaction] persist event : " ++ (show event)
    let eventStream = (getEventStream streamRepository) $ getAggregateId (event::Event)
    persist eventStream event
    transactionInterpreterForEventStore logger streamRepository next

transactionInterpreterForEventStore logger streamRepository (Free (PersistValidationState validationState next)) = do
    liftIO $ logInfo logger $ "[command.transaction] persist validationState : " ++ (show validationState)
    let validationStateStream = (getValidationStateStream streamRepository) $ getAggregateId validationState
    persist validationStateStream validationState
    transactionInterpreterForEventStore logger streamRepository next

transactionInterpreterForEventStore logger streamRepository (Free (PersistCommandResponse commandResponse next))  = do
    liftIO $ logInfo logger $ "[command.transaction] persist response : " ++ (show commandResponse)
    let responseStream = (getCommandResponseStream streamRepository) $ getAggregateId commandResponse
    persist responseStream commandResponse
    transactionInterpreterForEventStore logger streamRepository next

transactionInterpreterForEventStore logger streamRepository (Free (GetCurrentTime fct))  = do
    now <- Time.getCurrentTime
    transactionInterpreterForEventStore logger streamRepository (fct now)

transactionInterpreterForEventStore logger streamRepository (Free (GetNewEventId fct))  = do
    eventId <- liftIO $ Uuid.nextRandom
    transactionInterpreterForEventStore logger streamRepository (fct eventId)