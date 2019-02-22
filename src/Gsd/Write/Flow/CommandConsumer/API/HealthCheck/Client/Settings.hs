{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Write.Flow.CommandConsumer.API.HealthCheck.Client.Settings where

import Logger.Core
import Network.Core

data Settings = Settings {loggerId :: LoggerId , url :: URL}