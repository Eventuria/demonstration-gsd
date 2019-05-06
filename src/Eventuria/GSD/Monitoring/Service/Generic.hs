{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Eventuria.GSD.Monitoring.Service.Generic (
                streamWorkspaceId,
                streamCommand,
                streamCommandResponse,
                streamEvent,
                streamWriteModelHistory) where

import           Control.Lens
import           Control.Exception

import           Data.Function ((&))

import           Streamly hiding (Streaming)
import qualified Streamly.Prelude as S

import           Eventuria.Libraries.PersistedStreamEngine.Interface.Streamable
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Read.Reading
import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem

import           Eventuria.Libraries.CQRS.Write.Aggregate.Events.Event
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import           Eventuria.Libraries.CQRS.Write.StreamRepository
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command

import           Eventuria.Libraries.CQRS.Read.StreamRepository

import           Eventuria.GSD.Write.Model.WriteModel
import           Eventuria.GSD.Write.Model.Commands.Command
import           Eventuria.GSD.Write.Model.Commands.Mapper
import           Eventuria.GSD.Write.Model.Events.Event
import           Eventuria.GSD.Write.Model.Core

streamWorkspaceId :: StreamAll WorkspaceId ->
                      SerialT IO (Either SomeException (Persisted WorkspaceId))
streamWorkspaceId streamAll = streamAll

streamCommand ::  Streamable stream monad Command => GetCommandStream persistedStream ->
                    Streaming persistedStream ->
                    WorkspaceId ->
                    stream monad (Either SomeException (Persisted GSDCommand))
streamCommand getCommandStream Streaming {streamAll} workspaceId =
  (streamAll $ getCommandStream workspaceId) &
      S.map (\result -> over _Right (\PersistedItem { offset = offset, item = cqrsCommand} ->
                                      PersistedItem { offset = offset, item = fromCommand $ cqrsCommand}) result)

streamEvent :: GetStreamAll Event ->
               WorkspaceId ->
               SerialT IO (Either SomeException (Persisted GsdEvent))
streamEvent getStreamAllEventsByAggregateId workspaceId =
  (getStreamAllEventsByAggregateId workspaceId) &
      S.map (\result -> over _Right (\PersistedItem { offset = offset, item = cqrsEvent} ->
              PersistedItem { offset = offset, item = fromEvent $ cqrsEvent}) result)


streamCommandResponse :: GetStreamAll CommandResponse ->
                         WorkspaceId ->
                         SerialT IO (Either SomeException (Persisted CommandResponse))
streamCommandResponse getStreamAll = getStreamAll


streamWriteModelHistory :: GetStreamAll (Maybe GsdWriteModel) ->
                           WorkspaceId ->
                          SerialT IO (Either SomeException (Persisted (Maybe GsdWriteModel)))
streamWriteModelHistory getStreamAll = getStreamAll



