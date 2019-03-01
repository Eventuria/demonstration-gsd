{-# LANGUAGE FlexibleContexts #-}
module Eventuria.GSD.Read.Service.OverEventStore where

import           Control.Exception

import           Streamly (SerialT)

import           Eventuria.Libraries.CQRS.Write.StreamRepository
import           Eventuria.Libraries.CQRS.Write.Aggregate.Events.Event

import           Eventuria.Libraries.PersistedStreamEngine.Interface.Streamable
import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import           Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Dependencies
import           Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Read.CqrsInstance

import           Eventuria.GSD.Write.Repository.EventStoreStreams
import           Eventuria.GSD.Write.Model.Core

import qualified Eventuria.GSD.Read.Service.Generic  as GenericRead
import           Eventuria.GSD.Read.Model.Workspace
import           Eventuria.GSD.Read.Model.Goal
import           Eventuria.GSD.Read.Model.Action

streamWorkspace :: (Streamable stream monad WorkspaceId , Streamable SerialT monad Event) =>
                      Dependencies ->
                      stream monad (Either SomeException (Persisted Workspace))
streamWorkspace settings =
    GenericRead.streamWorkspace
      (aggregateIdStream $ getEventStoreStreamRepository settings)
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming

fetchWorkspace :: Dependencies -> WorkspaceId -> IO (Either SomeException  (Maybe Workspace))
fetchWorkspace settings workspaceId =
    GenericRead.fetchWorkspace
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId

streamGoal :: Streamable stream monad Event =>
                Dependencies ->
                WorkspaceId ->
                stream monad (Either SomeException  Goal)
streamGoal settings workspaceId =
    GenericRead.streamGoal
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId

fetchGoal :: Dependencies ->
             WorkspaceId ->
             GoalId ->
             IO (Either SomeException  (Maybe Goal))
fetchGoal settings workspaceId goalId =
    GenericRead.fetchGoal
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId
      goalId

streamAction :: Streamable stream monad Event =>
                  Dependencies ->
                  WorkspaceId ->
                  GoalId ->
                  stream monad (Either SomeException  (Action))
streamAction settings workspaceId goalId =
    GenericRead.streamAction
      (getEventStream $ getEventStoreStreamRepository settings)
      getEventStoreStreaming
      workspaceId
      goalId
