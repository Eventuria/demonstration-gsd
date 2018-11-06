module Cqrs.CommandHandler where

import Cqrs.Logger
import qualified Database.EventStore as EventStore
import Cqrs.PersistedCommand
import Cqrs.Snapshot

type CommandHandler = Logger -> EventStore.Connection -> PersistedCommand -> Maybe AggregateSnapshot -> IO ()
