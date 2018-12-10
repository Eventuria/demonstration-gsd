module PersistedStreamEngine.Instances.EventStore.CqrsEDSLInterpreter where

import Control.Monad.Free
import qualified Data.Time as Time
import qualified Data.UUID.V4 as Uuid
import Control.Monad.IO.Class (MonadIO(..))

import Logger.Core

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



interpretWriteEventStoreLanguage :: InterpreterWritePersistedStreamLanguage EventStoreStream a

interpretWriteEventStoreLanguage (Pure a) logger streamRepository = return a
interpretWriteEventStoreLanguage (Free (PersistEvent event next)) logger streamRepository = do
    liftIO $ logInfo logger  $ "persist event : " ++ (show event)
    let eventStream = (getEventStream streamRepository) $ getAggregateId (event::Event)
    persist eventStream event
    interpretWriteEventStoreLanguage next logger streamRepository
interpretWriteEventStoreLanguage (Free (PersistValidationState validationState next)) logger streamRepository = do
    liftIO $ logInfo logger $ "persist validationState : " ++ (show validationState)
    let validationStateStream = (getValidationStateStream streamRepository) $ getAggregateId validationState
    persist validationStateStream validationState
    interpretWriteEventStoreLanguage next logger streamRepository
interpretWriteEventStoreLanguage (Free (PersistCommandResponse commandResponse next)) logger streamRepository = do
    liftIO $ logInfo logger $ "persist response : " ++ (show commandResponse)
    let responseStream = (getCommandResponseStream streamRepository) $ getAggregateId commandResponse
    persist responseStream commandResponse
    interpretWriteEventStoreLanguage next logger streamRepository
interpretWriteEventStoreLanguage (Free (GetCurrentTime fct)) logger streamRepository = do
    now <- Time.getCurrentTime
    interpretWriteEventStoreLanguage (fct now) logger streamRepository
interpretWriteEventStoreLanguage (Free (GetNewEventId fct)) logger streamRepository = do
    eventId <- liftIO $ Uuid.nextRandom
    interpretWriteEventStoreLanguage (fct eventId) logger streamRepository