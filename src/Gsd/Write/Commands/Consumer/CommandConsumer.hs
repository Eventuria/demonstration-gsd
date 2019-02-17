{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Write.Commands.Consumer.CommandConsumer where

import Logger.Core
import qualified Gsd.Write.GsdOverEventStore as Gsd.Write
import qualified Gsd.Write.Commands.Consumer.State as Consumer.State
import Gsd.Write.Commands.Consumer.Settings
import Gsd.Write.Commands.Consumer.State

start :: Settings -> IO ()
start settings  =
  Consumer.State.getState
      settings
      (\State {logger, eventStoreClientState } -> do
            logInfo logger "Starting Command Consumer"
            Gsd.Write.startCommandConsumption
               eventStoreClientState
               logger
            return ())

