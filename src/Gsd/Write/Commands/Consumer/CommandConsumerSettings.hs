{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Write.Commands.Consumer.CommandConsumerSettings where

import Logger.Core
import PersistedStreamEngine.Instances.EventStore.EventStoreClientSettings

data CommandConsumerSettings = CommandConsumerSettings {
                        eventStoreClientSettings :: EventStoreClientSettings,
                        logger :: Logger }

getSettings :: LoggerId -> EventStoreClientSettings -> IO(CommandConsumerSettings)
getSettings loggerId eventStoreClientSettings = do
  logger <- getLogger loggerId
  return CommandConsumerSettings {..}