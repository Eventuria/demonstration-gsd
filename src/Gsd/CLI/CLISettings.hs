{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.CLI.CLISettings where

import Logger.Core
import qualified Gsd.Read.API.Client.Settings as Read
import qualified Gsd.Write.API.Client.Settings as Write
import qualified Gsd.Monitoring.API.Client.Settings as Monitoring

data CLISettings = CLISettings {
                        logger :: Logger,
                        writeClient :: Write.ClientSettings,
                        readClient :: Read.ClientSettings,
                        monitoringClient :: Monitoring.ClientSettings}

getSettings :: LoggerId ->
               Write.ClientSettings ->
               Read.ClientSettings ->
               Monitoring.ClientSettings -> IO(CLISettings)
getSettings loggerId writeClient readClient monitoringClient = do
  logger <- getLogger loggerId
  return CLISettings {..}