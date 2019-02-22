{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.CLI.Settings where

import Logger.Core
import qualified Gsd.Read.API.Client.Settings as Read.Client
import qualified Gsd.Write.Flow.Sourcer.Client.Settings as Write.Client
import qualified Gsd.Write.Flow.CommandConsumer.API.HealthCheck.Client.Settings as Write.Command.Consumer.Client
import qualified Gsd.Monitoring.API.Client.Settings as Monitoring.Client

data Settings = Settings {
                        loggerId :: LoggerId,
                        writeClientSettings :: Write.Client.Settings,
                        writeCommandConsumerClientSettings :: Write.Command.Consumer.Client.Settings,
                        readClientSettings :: Read.Client.Settings,
                        monitoringClientSettings :: Monitoring.Client.Settings}

