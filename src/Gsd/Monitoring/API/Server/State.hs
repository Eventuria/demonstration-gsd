{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Monitoring.API.Server.State where

import Logger.Core
import Network.Core
import PersistedStreamEngine.Instances.EventStore.EventStoreClientState
import qualified PersistedStreamEngine.Instances.EventStore.EventStoreClientState as EventStoreClientState
import Gsd.Monitoring.API.Server.Settings

data State = State { logger :: Logger ,
                     port :: URLPort,
                     eventStoreClientState :: EventStoreClientState}

getState :: Settings -> (State -> IO c) -> IO c
getState Settings {loggerId, eventStoreClientSettings,port} executionUnderResourceManagement = do
  logger <- getLogger loggerId
  EventStoreClientState.getState
    eventStoreClientSettings
    (\eventStoreClientState -> executionUnderResourceManagement State {..} )
