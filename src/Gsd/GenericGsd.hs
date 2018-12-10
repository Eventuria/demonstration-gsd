{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Gsd.GenericGsd (requestCommand,
                streamCommands,
                streamWorkspaceIds,
                runCommandConsumers) where


import Data.Maybe

import Data.Function ((&))
import qualified Streamly.Prelude as S

import Streamly.Streamable

import Logger.Core

import qualified Cqrs.Write.CommandConsumerFlow as CommandConsumerFlow
import qualified Cqrs.Cqrs as Cqrs
import Cqrs.Write.StreamRepository
import PersistedStreamEngine.PersistedItem
import Cqrs.Write.Aggregate.Commands.Command
import PersistedStreamEngine.Write.Interface
import PersistedStreamEngine.Read.Interface
import PersistedStreamEngine.Write.WDsl

import Gsd.CommandHandler
import Gsd.Commands
import Gsd.Core

import PersistedStreamEngine.Write.PersistenceResult

requestCommand ::  CqrsStreamRepository persistedStream -> Querying persistedStream -> Writing persistedStream -> GsdCommand -> IO PersistenceResult
requestCommand cqrsStreamRepository querying writing gsdCommand =
  Cqrs.persistCommands
    writing
    querying
    (getCommandStream  $ cqrsStreamRepository)
    (aggregateIdStream $ cqrsStreamRepository) $ toCommand gsdCommand

runCommandConsumers :: CqrsStreamRepository persistedStream -> Reading persistedStream -> InterpreterWritePersistedStreamLanguage persistedStream () -> Logger ->  IO ()
runCommandConsumers cqrsStreamRepository reading interpreterWritePersistedStreamLanguage logger  =
   CommandConsumerFlow.runCommandConsumers
      logger
      cqrsStreamRepository
      reading
      gsdCommandHandler
      interpreterWritePersistedStreamLanguage

streamWorkspaceIds :: Streamable stream monad WorkspaceId => CqrsStreamRepository persistedStream -> Streaming persistedStream -> stream monad (Persisted WorkspaceId)
streamWorkspaceIds cqrsStreamRepository Streaming {streamAll} = streamAll $ aggregateIdStream cqrsStreamRepository


streamCommands ::  Streamable stream monad Command => CqrsStreamRepository persistedStream -> Streaming persistedStream -> WorkspaceId -> stream monad (Persisted GsdCommand)
streamCommands cqrsStreamRepository Streaming {streamAll} workspaceId = do
  (streamAll $ (getCommandStream cqrsStreamRepository) workspaceId) &
      S.map (\PersistedItem { offset = offset, item = cqrsCommand} ->
              PersistedItem { offset = offset, item = fromJust $ fromCommand $ cqrsCommand})



