{-# LANGUAGE FlexibleContexts #-}
module Gsd.Read.ReadOverEventStore (streamWorkspaces) where

import qualified Gsd.Read.GenericRead  as GenericRead
import PersistedStreamEngine.Interface.Streamable

import PersistedStreamEngine.Interface.PersistedItem
import PersistedStreamEngine.Instances.EventStore.EventStoreSettings
import PersistedStreamEngine.Instances.EventStore.Read.CqrsInstance

import Gsd.Write.EventStoreStreamRepository
import Gsd.Write.Core
import Gsd.Read.Workspace
import Cqrs.Write.StreamRepository

streamWorkspaces :: Streamable stream monad WorkspaceId => EventStoreSettings -> stream monad (Persisted Workspace)
streamWorkspaces settings =
    GenericRead.streamWorkspaces
      (aggregateIdStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
