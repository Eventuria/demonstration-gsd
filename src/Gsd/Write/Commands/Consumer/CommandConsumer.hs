{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Write.Commands.Consumer.CommandConsumer where

import Logger.Core
import PersistedStreamEngine.Instances.EventStore.EventStoreClientState
import qualified Gsd.Write.GsdOverEventStore as Gsd.Write

import Gsd.Write.Commands.Consumer.CommandConsumerSettings

start :: CommandConsumerSettings -> IO ()
start CommandConsumerSettings {eventStoreClientSettings,logger} = do
  logInfo logger "Starting Command Consumer"
  getState
    eventStoreClientSettings
    (\eventStoreClientState -> do
          Gsd.Write.startCommandConsumption
             eventStoreClientState
             logger
          return ())

