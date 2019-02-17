{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Monitoring.API.Server.Settings where

import Logger.Core
import Network.Core
import PersistedStreamEngine.Instances.EventStore.EventStoreClientSettings


data ServerSettings = ServerSettings {
                        port :: URLPort,
                        eventStoreClientSettings :: EventStoreClientSettings,
                        logger :: Logger }

getSettings :: LoggerId -> URLPort -> EventStoreClientSettings -> IO(ServerSettings)
getSettings loggerId port eventStoreClientSettings = do
  logger <- getLogger loggerId
  return ServerSettings {port ,..}