{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Monitoring.API.Client.Settings where

import Servant.Client
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Logger.Core
import Network.Core

data ClientSettings = ClientSettings {
                        url :: BaseUrl,
                        httpClientManager :: Manager,
                        logger :: Logger }

getSettings :: LoggerId -> URLHost -> URLPort -> URLPath -> IO(ClientSettings)
getSettings monitoringLoggerId urlHost urlPort urlPath = do
  httpClientManager <- (newManager defaultManagerSettings)
  logger <- getLogger monitoringLoggerId
  return ClientSettings {url = BaseUrl Http urlHost urlPort urlPath ,..}