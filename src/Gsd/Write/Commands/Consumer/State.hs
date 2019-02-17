{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Write.Commands.Consumer.State where

import Logger.Core
import PersistedStreamEngine.Instances.EventStore.EventStoreClientState
import qualified PersistedStreamEngine.Instances.EventStore.EventStoreClientState as EventStoreClientState
import Gsd.Write.Commands.Consumer.Settings

data State = State {logger :: Logger ,
                    eventStoreClientState :: EventStoreClientState}

getState :: Settings -> (State -> IO c) -> IO c
getState Settings {loggerId, eventStoreClientSettings} executionUnderResourceManagement = do
  logger <- getLogger loggerId
  EventStoreClientState.getState
    eventStoreClientSettings
    (\eventStoreClientState -> executionUnderResourceManagement State {..} )