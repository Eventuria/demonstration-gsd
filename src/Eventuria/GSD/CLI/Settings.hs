{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.GSD.CLI.Settings where

import Eventuria.Commons.Logger.Core
import qualified Eventuria.GSD.Read.API.Client.Settings as Read.Client
import qualified Eventuria.GSD.Write.CommandSourcer.Client.Settings as Write.Client
import qualified Eventuria.GSD.Write.CommandConsumer.API.HealthCheck.Client.Settings as Write.Command.Consumer.Client
import qualified Eventuria.GSD.Monitoring.API.Client.Settings as Monitoring.Client

data Settings = Settings {
                        loggerId :: LoggerId,
                        commandSourcerClientSettings :: Write.Client.Settings,
                        commandConsumerClientSettings :: Write.Command.Consumer.Client.Settings,
                        readClientSettings :: Read.Client.Settings,
                        monitoringClientSettings :: Monitoring.Client.Settings}

