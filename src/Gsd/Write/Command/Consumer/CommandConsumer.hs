{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Write.Command.Consumer.CommandConsumer where

import Logger.Core
import qualified Gsd.Write.Service.OverEventStore as Gsd.Write
import qualified Gsd.Write.Command.Consumer.State as Consumer.State
import Gsd.Write.Command.Consumer.Settings
import Gsd.Write.Command.Consumer.State

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

