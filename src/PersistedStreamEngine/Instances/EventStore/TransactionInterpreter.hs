module PersistedStreamEngine.Instances.EventStore.TransactionInterpreter where

import Control.Monad.Free
import qualified Data.Time as Time
import qualified Data.UUID.V4 as Uuid
import Control.Monad.IO.Class (MonadIO(..))

import Logger.Core
import Data.Aeson
import PersistedStreamEngine.Interface.Write.WDsl
import Cqrs.Write.Aggregate.Events.Event
import Cqrs.Write.Aggregate.Core
import Cqrs.Write.StreamRepository

import PersistedStreamEngine.Instances.EventStore.Write.Persisting
import PersistedStreamEngine.Instances.EventStore.EventStoreStream

import Cqrs.Write.Serialization.Command ()
import Cqrs.Write.Serialization.AggregateId ()
import Cqrs.Write.Serialization.Event ()
import Cqrs.Write.Serialization.ValidationState ()
import Cqrs.Write.Serialization.CommandResponse ()



transactionInterpreterForEventStore :: (ToJSON applicationState , Show applicationState) =>
                                        Logger ->
                                        CqrsStreamRepository EventStoreStream applicationState ->
                                        TransactionInterpreter applicationState a

transactionInterpreterForEventStore logger streamRepository (Pure a) = return $ Right a
transactionInterpreterForEventStore logger streamRepository (Free (PersistEvent event next))  = do
    liftIO $ logInfo logger  $ "persist event : " ++ (show event)
    let eventStream = (getEventStream streamRepository) $ getAggregateId (event::Event)
    persist eventStream event
    transactionInterpreterForEventStore logger streamRepository next
transactionInterpreterForEventStore logger streamRepository (Free (PersistValidationState validationState next)) = do
    liftIO $ logInfo logger $ "persist validationState : " ++ (show validationState)
    let validationStateStream = (getValidationStateStream streamRepository) $ getAggregateId validationState
    persist validationStateStream validationState
    transactionInterpreterForEventStore logger streamRepository next
transactionInterpreterForEventStore logger streamRepository (Free (PersistCommandResponse commandResponse next))  = do
    liftIO $ logInfo logger $ "persist response : " ++ (show commandResponse)
    let responseStream = (getCommandResponseStream streamRepository) $ getAggregateId commandResponse
    persist responseStream commandResponse
    transactionInterpreterForEventStore logger streamRepository next
transactionInterpreterForEventStore logger streamRepository (Free (GetCurrentTime fct))  = do
    now <- Time.getCurrentTime
    transactionInterpreterForEventStore logger streamRepository (fct now)
transactionInterpreterForEventStore logger streamRepository (Free (GetNewEventId fct))  = do
    eventId <- liftIO $ Uuid.nextRandom
    transactionInterpreterForEventStore logger streamRepository (fct eventId)