{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Eventuria.GSD.Write.CommandSourcer.Client.Dependencies where

import Servant.Client
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Eventuria.Commons.Logger.Core
import Eventuria.GSD.Write.CommandSourcer.Client.Settings
import Eventuria.Adapters.Servant.Adapter

data Dependencies = Dependencies {logger :: Logger ,
                    url :: BaseUrl,
                    httpClientManager :: Manager}

getDependencies :: Settings -> IO(Dependencies)
getDependencies Settings { url, loggerId} = do
  logger <- getLogger loggerId
  httpClientManager <- (newManager defaultManagerSettings)
  return Dependencies {url = toBaseUrl url ,..}



