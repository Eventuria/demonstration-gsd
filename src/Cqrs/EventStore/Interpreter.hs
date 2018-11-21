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

interpret :: EventStoreLanguage a -> Logger -> EventStore.Credentials -> EventStore.Connection -> IO a

interpret (Pure a) logger credentials connection = return a
interpret (Free (PersistEvent event next)) logger credentials connection = do
    EventStream.persist logger credentials connection event
    interpret next logger credentials connection
interpret (Free (PersistAggregate aggregateSnapshot next)) logger credentials connection = do
    SnapshotStream.persist logger credentials connection aggregateSnapshot
    interpret next logger credentials connection
interpret (Free (PersistCommandResponse commandResponse next)) logger credentials connection = do
    CommandResponseStream.persist logger credentials connection commandResponse
    interpret next logger credentials connection
interpret (Free (GetCurrentTime fct)) logger credentials connection = do
    now <- Time.getCurrentTime
    interpret (fct now) logger credentials connection
interpret (Free (GetNewEventId fct)) logger credentials connection = do
    eventId <- liftIO $ Uuid.nextRandom
    interpret (fct eventId) logger credentials connection