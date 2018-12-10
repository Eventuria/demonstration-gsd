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

import Gsd.Write.CommandHandler
import Gsd.Write.Commands


import PersistedStreamEngine.Interface.Write.PersistenceResult

persistCommand ::  CqrsStreamRepository persistedStream -> Querying persistedStream -> Writing persistedStream -> GsdCommand -> IO PersistenceResult
persistCommand cqrsStreamRepository querying writing gsdCommand =
  Cqrs.Write.persistCommand
    writing
    querying
    (getCommandStream  $ cqrsStreamRepository)
    (aggregateIdStream $ cqrsStreamRepository) $ toCommand gsdCommand

streamCommandConsumption :: CqrsStreamRepository persistedStream -> Reading persistedStream -> InterpreterWritePersistedStreamLanguage persistedStream () -> Logger ->  IO ()
streamCommandConsumption cqrsStreamRepository reading interpreterWritePersistedStreamLanguage logger  =
   Cqrs.Write.CommandConsumption.stream
      logger
      cqrsStreamRepository
      reading
      gsdCommandHandler
      interpreterWritePersistedStreamLanguage




