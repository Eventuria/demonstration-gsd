module PersistedStreamEngine.Instances.EventStore.CqrsEDSLInterpreter where

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


interpretWriteEventStoreLanguage :: (ToJSON applicationState , Show applicationState) =>
                                        Logger ->
                                        CqrsStreamRepository EventStoreStream applicationState ->
                                        TransactionInterpreter applicationState a

interpretWriteEventStoreLanguage logger streamRepository (Pure a) = return $ Right a
interpretWriteEventStoreLanguage logger streamRepository (Free (PersistEvent event next))  = do
    liftIO $ logInfo logger  $ "persist event : " ++ (show event)
    let eventStream = (getEventStream streamRepository) $ getAggregateId (event::Event)
    persist eventStream event
    interpretWriteEventStoreLanguage logger streamRepository next
interpretWriteEventStoreLanguage logger streamRepository (Free (PersistValidationState validationState next)) = do
    liftIO $ logInfo logger $ "persist validationState : " ++ (show validationState)
    let validationStateStream = (getValidationStateStream streamRepository) $ getAggregateId validationState
    persist validationStateStream validationState
    interpretWriteEventStoreLanguage logger streamRepository next
interpretWriteEventStoreLanguage logger streamRepository (Free (PersistCommandResponse commandResponse next))  = do
    liftIO $ logInfo logger $ "persist response : " ++ (show commandResponse)
    let responseStream = (getCommandResponseStream streamRepository) $ getAggregateId commandResponse
    persist responseStream commandResponse
    interpretWriteEventStoreLanguage logger streamRepository next
interpretWriteEventStoreLanguage logger streamRepository (Free (GetCurrentTime fct))  = do
    now <- Time.getCurrentTime
    interpretWriteEventStoreLanguage logger streamRepository (fct now)
interpretWriteEventStoreLanguage logger streamRepository (Free (GetNewEventId fct))  = do
    eventId <- liftIO $ Uuid.nextRandom
    interpretWriteEventStoreLanguage logger streamRepository (fct eventId)