{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Gsd.Read.Monitoring.GenericMonitoring (
                streamCommands,
                streamWorkspaceIds) where


import Data.Maybe

import Data.Function ((&))
import qualified Streamly.Prelude as S

import Streamly.Streamable
import Cqrs.Write.StreamRepository
import PersistedStreamEngine.PersistedItem
import Cqrs.Write.Aggregate.Commands.Command

import PersistedStreamEngine.Read.Interface

import Gsd.Write.Commands
import Gsd.Write.Core

streamWorkspaceIds :: Streamable stream monad WorkspaceId => CqrsStreamRepository persistedStream -> Streaming persistedStream -> stream monad (Persisted WorkspaceId)
streamWorkspaceIds cqrsStreamRepository Streaming {streamAll} = streamAll $ aggregateIdStream cqrsStreamRepository


streamCommands ::  Streamable stream monad Command => CqrsStreamRepository persistedStream -> Streaming persistedStream -> WorkspaceId -> stream monad (Persisted GsdCommand)
streamCommands cqrsStreamRepository Streaming {streamAll} workspaceId = do
  (streamAll $ (getCommandStream cqrsStreamRepository) workspaceId) &
      S.map (\PersistedItem { offset = offset, item = cqrsCommand} ->
              PersistedItem { offset = offset, item = fromJust $ fromCommand $ cqrsCommand})



