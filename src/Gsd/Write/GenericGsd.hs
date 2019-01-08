{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Gsd.Write.GenericGsd (persistCommand,streamCommandConsumption) where

import Logger.Core

import qualified Cqrs.Write.CommandConsumption.Stream as Cqrs.Write.CommandConsumption
import qualified Cqrs.Write.CqrsWrite as Cqrs.Write
import Cqrs.Write.StreamRepository

import PersistedStreamEngine.Interface.Write.Writing
import PersistedStreamEngine.Interface.Read.Reading
import PersistedStreamEngine.Interface.Write.WDsl

import Gsd.Write.Commands.Handling.CommandHandler (commandHandler)
import Gsd.Write.Commands.Command
import Gsd.Write.State

import PersistedStreamEngine.Interface.Write.PersistenceResult

persistCommand ::  AggregateIdStream persistedStream -> GetCommandStream persistedStream->  Querying persistedStream -> Writing persistedStream -> GsdCommand -> IO PersistenceResult
persistCommand aggregateIdStream getCommandStream querying writing gsdCommand =
  Cqrs.Write.persistCommand
    writing
    querying
    getCommandStream
    aggregateIdStream $ toCommand gsdCommand

streamCommandConsumption :: CqrsStreamRepository persistedStream GsdState -> Reading persistedStream -> InterpreterWritePersistedStreamLanguage persistedStream GsdState () -> Logger ->  IO ()
streamCommandConsumption cqrsStreamRepository reading interpreterWritePersistedStreamLanguage logger  =
   Cqrs.Write.CommandConsumption.stream
      logger
      cqrsStreamRepository
      reading
      commandHandler
      interpreterWritePersistedStreamLanguage




