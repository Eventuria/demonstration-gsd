{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Write.Flow.CommandConsumer.API.HealthCheck.Client.Settings where

import Eventuria.Commons.Logger.Core
import Eventuria.Commons.Network.Core

data Settings = Settings {loggerId :: LoggerId , url :: URL}