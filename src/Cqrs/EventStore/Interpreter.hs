module Cqrs.EventStore.Interpreter where

import Cqrs.Logger
import qualified Database.EventStore as EventStore
import qualified Cqrs.CommandResponseStream as CommandResponseStream
import qualified Cqrs.SnapshotStream as SnapshotStream
import qualified Cqrs.EventStream as EventStream
import Control.Monad.Free
import Cqrs.Events
import qualified Data.Time as Time
import qualified Data.UUID.V4 as Uuid
import Control.Monad.IO.Class (MonadIO(..))
import Cqrs.EventStore.EDsl

interpret :: EventStoreLanguage a -> Logger -> EventStore.Connection -> IO a

interpret (Pure a) logger connection = return a
interpret (Free (PersistEvent event next)) logger connection = do
    EventStream.persist logger connection event
    interpret next logger connection
interpret (Free (PersistAggregate aggregateSnapshot next)) logger connection = do
    SnapshotStream.persist logger connection aggregateSnapshot
    interpret next logger connection
interpret (Free (PersistCommandResponse commandResponse next)) logger connection = do
    CommandResponseStream.persist logger connection commandResponse
    interpret next logger connection
interpret (Free (GetCurrentTime fct)) logger connection = do
    now <- Time.getCurrentTime
    interpret (fct now) logger connection
interpret (Free (GetNewEventId fct)) logger connection = do
    eventId <- liftIO $ Uuid.nextRandom
    interpret (fct eventId) logger connection