module Plugins.GregYoungEventStore.InterpreterEventStore where

import Control.Monad.Free
import qualified Data.Time as Time
import qualified Data.UUID.V4 as Uuid
import Control.Monad.IO.Class (MonadIO(..))

import Logger.Core

import Cqrs.PersistedStream.Write.WDsl
import Cqrs.Aggregate.Events.Event
import Cqrs.Aggregate.Core
import Cqrs.Aggregate.StreamRepository

import Plugins.GregYoungEventStore.Write.Persisting
import Plugins.GregYoungEventStore.Stream


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