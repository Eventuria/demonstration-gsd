{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.GSD.Read.API.Client.Dependencies where

import Servant.Client
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Eventuria.Commons.Logger.Core
import Eventuria.GSD.Read.API.Client.Settings
import Eventuria.Adapters.Servant.Adapter

data Dependencies = Dependencies {logger :: Logger ,
                    url :: BaseUrl,
                    httpClientManager :: Manager}

getDependencies :: Settings -> IO(Dependencies)
getDependencies Settings { url, loggerId} = do
  logger <- getLogger loggerId
  httpClientManager <- (newManager defaultManagerSettings)
  return Dependencies {url = toBaseUrl url,..}

