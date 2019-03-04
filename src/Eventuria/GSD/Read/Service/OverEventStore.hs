{-# LANGUAGE FlexibleContexts #-}
module Eventuria.GSD.Read.Service.OverEventStore where

import           Control.Exception

import           Streamly (SerialT)

import           Eventuria.Libraries.CQRS.Write.StreamRepository
import           Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Read.ReadProjections
import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import           Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Dependencies

import           Eventuria.GSD.Write.Repository.EventStoreStreams
import           Eventuria.GSD.Write.Model.Core

import qualified Eventuria.GSD.Read.Service.Generic  as GenericRead
import           Eventuria.GSD.Read.Model.Workspace
import           Eventuria.GSD.Read.Model.Goal
import           Eventuria.GSD.Read.Model.Action

streamWorkspace :: Dependencies ->
                   SerialT IO (Either SomeException (Persisted Workspace))
streamWorkspace settings =
    GenericRead.streamWorkspace
      (streamAllAggregateId (aggregateIdStream $ getEventStoreStreamRepository settings))
      (getStreamAllEventsByAggregateId (getCommandTransactionStream $ getEventStoreStreamRepository settings))

fetchWorkspace :: Dependencies -> WorkspaceId -> IO (Either SomeException  (Maybe Workspace))
fetchWorkspace settings workspaceId =
    GenericRead.fetchWorkspace
      (getStreamAllEventsByAggregateId (getCommandTransactionStream $ getEventStoreStreamRepository settings))
      workspaceId

streamGoal :: Dependencies ->
              WorkspaceId ->
              SerialT IO (Either SomeException  Goal)
streamGoal settings workspaceId =
    GenericRead.streamGoal
      (getStreamAllEventsByAggregateId (getCommandTransactionStream $ getEventStoreStreamRepository settings))
      workspaceId

fetchGoal :: Dependencies ->
             WorkspaceId ->
             GoalId ->
             IO (Either SomeException  (Maybe Goal))
fetchGoal settings workspaceId goalId =
    GenericRead.fetchGoal
      (getStreamAllEventsByAggregateId (getCommandTransactionStream $ getEventStoreStreamRepository settings))
      workspaceId
      goalId

streamAction :: Dependencies ->
                WorkspaceId ->
                GoalId ->
                SerialT IO (Either SomeException  (Action))
streamAction settings workspaceId goalId =
    GenericRead.streamAction
      (getStreamAllEventsByAggregateId (getCommandTransactionStream $ getEventStoreStreamRepository settings))
      workspaceId
      goalId
