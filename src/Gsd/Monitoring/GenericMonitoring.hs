{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Gsd.Monitoring.GenericMonitoring (
                streamWorkspaceIds,
                streamCommands,
                streamInfinitelyCommands,
                streamEvents,
                streamInfinitelyEvents) where

import Data.Function ((&))
import qualified Streamly.Prelude as S

import PersistedStreamEngine.Interface.Streamable
import Cqrs.Write.StreamRepository
import PersistedStreamEngine.Interface.PersistedItem
import Cqrs.Write.Aggregate.Commands.Command
import Cqrs.Write.Aggregate.Events.Event

import PersistedStreamEngine.Interface.Read.Reading

import Gsd.Write.Commands.Command
import Gsd.Write.Events.Event
import Gsd.Write.Core


streamWorkspaceIds :: Streamable stream monad WorkspaceId => CqrsStreamRepository persistedStream -> Streaming persistedStream -> stream monad (Persisted WorkspaceId)
streamWorkspaceIds cqrsStreamRepository Streaming {streamAll} = streamAll $ aggregateIdStream cqrsStreamRepository


streamCommands ::  Streamable stream monad Command => CqrsStreamRepository persistedStream -> Streaming persistedStream -> WorkspaceId -> stream monad (Persisted GsdCommand)
streamCommands cqrsStreamRepository Streaming {streamAll} workspaceId =
  (streamAll $ (getCommandStream cqrsStreamRepository) workspaceId) &
      S.map (\PersistedItem { offset = offset, item = cqrsCommand} ->
              PersistedItem { offset = offset, item = fromCommand $ cqrsCommand})

streamInfinitelyCommands ::  Streamable stream monad Command => CqrsStreamRepository persistedStream -> Streaming persistedStream -> WorkspaceId -> stream monad (Persisted GsdCommand)
streamInfinitelyCommands cqrsStreamRepository Streaming {streamAllInfinitely} workspaceId =
  (streamAllInfinitely $ (getCommandStream cqrsStreamRepository) workspaceId) &
      S.map (\PersistedItem { offset = offset, item = cqrsCommand} ->
              PersistedItem { offset = offset, item = fromCommand $ cqrsCommand})

streamEvents ::  Streamable stream monad Event => CqrsStreamRepository persistedStream -> Streaming persistedStream -> WorkspaceId -> stream monad (Persisted GsdEvent)
streamEvents cqrsStreamRepository Streaming {streamAll} workspaceId =
  (streamAll $ (getEventStream cqrsStreamRepository) workspaceId) &
      S.map (\PersistedItem { offset = offset, item = cqrsEvent} ->
              PersistedItem { offset = offset, item = fromEvent $ cqrsEvent})

streamInfinitelyEvents ::  Streamable stream monad Event => CqrsStreamRepository persistedStream -> Streaming persistedStream -> WorkspaceId -> stream monad (Persisted GsdEvent)
streamInfinitelyEvents cqrsStreamRepository Streaming {streamAllInfinitely} workspaceId =
  (streamAllInfinitely $ (getEventStream cqrsStreamRepository) workspaceId) &
      S.map (\PersistedItem { offset = offset, item = cqrsEvent} ->
              PersistedItem { offset = offset, item = fromEvent $ cqrsEvent})




