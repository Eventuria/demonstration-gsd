{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.GSD.Read.API.Client.Settings where

import Eventuria.Commons.Logger.Core
import Eventuria.Commons.Network.Core

data Settings = Settings {loggerId :: LoggerId , url :: URL}
