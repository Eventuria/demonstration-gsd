{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.GSD.CLI.Settings where

import Eventuria.Commons.Logger.Core
import qualified Eventuria.GSD.Read.API.Client.Settings as Read.Client
import qualified Eventuria.GSD.Write.CommandSourcer.Client.Settings as Command.Sourcer.Client
import qualified Eventuria.GSD.Write.CommandConsumer.API.HealthCheck.Client.Settings as Command.Consumer.Client
import qualified Eventuria.GSD.Monitoring.API.Client.Settings as Monitoring.Client

data Settings = Settings { loggerId :: LoggerId,
                           clientSettings :: ClientSettings }

data ClientSettings = ClientSettings { commandSourcer ::  Command.Sourcer.Client.Settings,
                                       commandConsumer :: Command.Consumer.Client.Settings,
                                       read :: Read.Client.Settings,
                                       monitoring :: Monitoring.Client.Settings}