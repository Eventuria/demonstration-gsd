module Cqrs.EventStore.Interpreter where

import qualified Cqrs.Aggregate.Commands.Responses.CommandResponseStream as CommandResponseStream
import qualified Cqrs.Aggregate.Commands.ValidationStates.ValidationStateStream as ValidationStateStream
import qualified Cqrs.Aggregate.Events.EventStream as EventStream
import Control.Monad.Free
import qualified Data.Time as Time
import qualified Data.UUID.V4 as Uuid
import Control.Monad.IO.Class (MonadIO(..))
import Cqrs.EventStore.EDsl
import Cqrs.EventStore.Context

interpret :: EventStoreLanguage a -> EventStoreContext -> IO a

interpret (Pure a) evenStoreContext = return a
interpret (Free (PersistEvent event next)) eventStoreContext = do
    EventStream.persist eventStoreContext event
    interpret next eventStoreContext
interpret (Free (PersistValidationState validationState next)) eventStoreContext = do
    ValidationStateStream.persist eventStoreContext validationState
    interpret next eventStoreContext
interpret (Free (PersistCommandResponse commandResponse next)) eventStoreContext = do
    CommandResponseStream.persist eventStoreContext commandResponse
    interpret next eventStoreContext
interpret (Free (GetCurrentTime fct)) eventStoreContext = do
    now <- Time.getCurrentTime
    interpret (fct now) eventStoreContext
interpret (Free (GetNewEventId fct)) eventStoreContext = do
    eventId <- liftIO $ Uuid.nextRandom
    interpret (fct eventId) eventStoreContext