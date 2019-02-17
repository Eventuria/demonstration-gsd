{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Gsd.Monitoring.Service.Generic (
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
import Gsd.Write.Model.State
import Gsd.Write.Model.Commands.Command
import Gsd.Write.Model.Events.Event
import Gsd.Write.Model.Core
import System.SafeResponse
import Control.Lens

streamWorkspaceId :: Streamable stream monad WorkspaceId =>
                      AggregateIdStream persistedStream ->
                      Streaming persistedStream ->
                      stream monad (SafeResponse (Persisted WorkspaceId))
streamWorkspaceId aggregateIdStream Streaming {streamAll} = streamAll aggregateIdStream


streamCommand ::  Streamable stream monad Command => GetCommandStream persistedStream ->
                    Streaming persistedStream ->
                    WorkspaceId ->
                    stream monad (SafeResponse (Persisted GsdCommand))
streamCommand getCommandStream Streaming {streamAll} workspaceId =
  (streamAll $ getCommandStream workspaceId) &
      S.map (\result -> over _Right (\PersistedItem { offset = offset, item = cqrsCommand} ->
                                      PersistedItem { offset = offset, item = fromCommand $ cqrsCommand}) result)


streamInfinitelyCommand ::  Streamable stream monad Command =>
                              GetCommandStream persistedStream ->
                              Streaming persistedStream ->
                              WorkspaceId ->
                              stream monad (SafeResponse (Persisted GsdCommand))
streamInfinitelyCommand getCommandStream Streaming {streamAllInfinitely} workspaceId =
  (streamAllInfinitely $ getCommandStream workspaceId) &
      S.map (\result -> over _Right (\PersistedItem { offset = offset, item = cqrsCommand} ->
              PersistedItem { offset = offset, item = fromCommand $ cqrsCommand}) result)

streamCommandResponse :: Streamable stream monad CommandResponse =>
                          GetCommandResponseStream persistedStream ->
                          Streaming persistedStream ->
                          WorkspaceId ->
                          stream monad (SafeResponse (Persisted CommandResponse))
streamCommandResponse getCommandResponseStream
                      Streaming {streamAll}
                      workspaceId = (streamAll $ getCommandResponseStream workspaceId)

streamEvent :: Streamable stream monad Event =>
                GetEventStream persistedStream ->
                Streaming persistedStream ->
                WorkspaceId ->
                stream monad (SafeResponse (Persisted GsdEvent))
streamEvent getEventStream Streaming {streamAll} workspaceId =
  (streamAll $ getEventStream workspaceId) &
      S.map (\result -> over _Right (\PersistedItem { offset = offset, item = cqrsEvent} ->
              PersistedItem { offset = offset, item = fromEvent $ cqrsEvent}) result)

streamInfinitelyEvent :: Streamable stream monad Event =>
                          GetEventStream persistedStream ->
                          Streaming persistedStream ->
                          WorkspaceId ->
                          stream monad (SafeResponse (Persisted GsdEvent))
streamInfinitelyEvent getEventStream Streaming {streamAllInfinitely} workspaceId =
  (streamAllInfinitely $ getEventStream  workspaceId) &
      S.map (\result -> over _Right (\PersistedItem { offset = offset, item = cqrsEvent} ->
              PersistedItem { offset = offset, item = fromEvent $ cqrsEvent}) result)

streamValidationState :: Streamable stream monad (ValidationState GsdState) =>
                          GetValidationStateStream persistedStream GsdState ->
                          Streaming persistedStream ->
                          WorkspaceId ->
                          stream monad (SafeResponse (Persisted (ValidationState GsdState)))
streamValidationState getValidateStateStream Streaming {streamAll} workspaceId =
  (streamAll $ getValidateStateStream  workspaceId)


