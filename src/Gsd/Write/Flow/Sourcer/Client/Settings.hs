{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Write.Flow.Sourcer.Client.Settings where

import Logger.Core
import Network.Core

data Settings = Settings {loggerId :: LoggerId , url :: URL}