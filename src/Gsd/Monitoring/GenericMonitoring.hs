{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Gsd.Monitoring.GenericMonitoring (
                streamWorkspaceId,
                streamCommand,
                streamInfinitelyCommand,
                streamCommandResponse,
                streamEvent,
                streamInfinitelyEvent,
                streamValidationState) where

import Data.Function ((&))
import qualified Streamly.Prelude as S

import PersistedStreamEngine.Interface.Streamable
import Cqrs.Write.StreamRepository
import PersistedStreamEngine.Interface.PersistedItem
import Cqrs.Write.Aggregate.Commands.Command
import Cqrs.Write.Aggregate.Events.Event
import Cqrs.Write.Aggregate.Commands.ValidationStates.ValidationState

import PersistedStreamEngine.Interface.Read.Reading
import Cqrs.Write.Aggregate.Commands.Responses.CommandResponse
import Gsd.Write.State
import Gsd.Write.Commands.Command
import Gsd.Write.Events.Event
import Gsd.Write.Core


streamWorkspaceId :: Streamable stream monad WorkspaceId => AggregateIdStream persistedStream -> Streaming persistedStream -> stream monad (Persisted WorkspaceId)
streamWorkspaceId aggregateIdStream Streaming {streamAll} = streamAll aggregateIdStream


streamCommand ::  Streamable stream monad Command => GetCommandStream persistedStream -> Streaming persistedStream -> WorkspaceId -> stream monad (Persisted GsdCommand)
streamCommand getCommandStream Streaming {streamAll} workspaceId =
  (streamAll $ getCommandStream workspaceId) &
      S.map (\PersistedItem { offset = offset, item = cqrsCommand} ->
              PersistedItem { offset = offset, item = fromCommand $ cqrsCommand})

streamInfinitelyCommand ::  Streamable stream monad Command => GetCommandStream persistedStream -> Streaming persistedStream -> WorkspaceId -> stream monad (Persisted GsdCommand)
streamInfinitelyCommand getCommandStream Streaming {streamAllInfinitely} workspaceId =
  (streamAllInfinitely $ getCommandStream workspaceId) &
      S.map (\PersistedItem { offset = offset, item = cqrsCommand} ->
              PersistedItem { offset = offset, item = fromCommand $ cqrsCommand})

streamCommandResponse ::  Streamable stream monad CommandResponse => GetCommandResponseStream persistedStream -> Streaming persistedStream -> WorkspaceId -> stream monad (Persisted CommandResponse)
streamCommandResponse getCommandResponseStream Streaming {streamAll} workspaceId = (streamAll $ getCommandResponseStream workspaceId)

streamEvent ::  Streamable stream monad Event => GetEventStream persistedStream -> Streaming persistedStream -> WorkspaceId -> stream monad (Persisted GsdEvent)
streamEvent getEventStream Streaming {streamAll} workspaceId =
  (streamAll $ getEventStream workspaceId) &
      S.map (\PersistedItem { offset = offset, item = cqrsEvent} ->
              PersistedItem { offset = offset, item = fromEvent $ cqrsEvent})

streamInfinitelyEvent ::  Streamable stream monad Event => GetEventStream persistedStream -> Streaming persistedStream -> WorkspaceId -> stream monad (Persisted GsdEvent)
streamInfinitelyEvent getEventStream Streaming {streamAllInfinitely} workspaceId =
  (streamAllInfinitely $ getEventStream  workspaceId) &
      S.map (\PersistedItem { offset = offset, item = cqrsEvent} ->
              PersistedItem { offset = offset, item = fromEvent $ cqrsEvent})

streamValidationState ::  Streamable stream monad (ValidationState GsdState) => GetValidateStateStream persistedStream GsdState -> Streaming persistedStream -> WorkspaceId -> stream monad (Persisted (ValidationState GsdState))
streamValidationState getValidateStateStream Streaming {streamAll} workspaceId =
  (streamAll $ getValidateStateStream  workspaceId)


