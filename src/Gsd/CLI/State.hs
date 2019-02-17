{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.CLI.State where

import Logger.Core
import qualified Gsd.Read.API.Client.State as Read.Client
import qualified Gsd.Write.API.Client.State as Write.Client
import qualified Gsd.Monitoring.API.Client.State as Monitoring.Client
import Gsd.CLI.Settings

data State = State {
                        logger :: Logger,
                        writeClientState :: Write.Client.State,
                        readClientState :: Read.Client.State,
                        monitoringClientState :: Monitoring.Client.State}

getState :: Settings -> IO(State)
getState Settings {loggerId,
                   writeClientSettings,
                   readClientSettings,
                   monitoringClientSettings} = do
  logger           <- getLogger loggerId
  writeClientState      <- Write.Client.getState      writeClientSettings
  readClientState       <- Read.Client.getState       readClientSettings
  monitoringClientState <- Monitoring.Client.getState monitoringClientSettings
  return State {..}