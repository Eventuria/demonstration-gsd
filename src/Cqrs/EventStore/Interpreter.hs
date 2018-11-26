module Cqrs.EventStore.Interpreter where

import Cqrs.Aggregate.Commands.Responses.CommandResponseStream
import Cqrs.Aggregate.Commands.ValidationStates.ValidationStateStream
import Cqrs.Aggregate.Events.EventStream as EventStream
import Control.Monad.Free
import qualified Data.Time as Time
import qualified Data.UUID.V4 as Uuid
import Control.Monad.IO.Class (MonadIO(..))
import Cqrs.EventStore.EDsl
import Cqrs.EventStore.Context
import Cqrs.EventStore.Writing
import Cqrs.Aggregate.Events.Event
import Cqrs.Aggregate.Core

interpret :: EventStoreLanguage a -> EventStoreContext -> IO a

interpret (Pure a) evenStoreContext = return a
interpret (Free (PersistEvent event next)) eventStoreContext = do
    let eventStream = getEventStream eventStoreContext $ getAggregateId (event::Event)
    persist eventStream event
    interpret next eventStoreContext
interpret (Free (PersistValidationState validationState next)) eventStoreContext = do
    let validateStateStream = getValidateStateStream eventStoreContext $ getAggregateId validationState
    persist validateStateStream validationState
    interpret next eventStoreContext
interpret (Free (PersistCommandResponse commandResponse next)) eventStoreContext = do
    let responseStream = getResponseStream eventStoreContext $ getAggregateId commandResponse
    persist responseStream commandResponse
    interpret next eventStoreContext
interpret (Free (GetCurrentTime fct)) eventStoreContext = do
    now <- Time.getCurrentTime
    interpret (fct now) eventStoreContext
interpret (Free (GetNewEventId fct)) eventStoreContext = do
    eventId <- liftIO $ Uuid.nextRandom
    interpret (fct eventId) eventStoreContext