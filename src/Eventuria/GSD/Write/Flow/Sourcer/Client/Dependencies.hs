{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.GSD.Write.Flow.Sourcer.Client.Dependencies where

import Servant.Client
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Eventuria.Commons.Logger.Core
import Eventuria.Commons.Network.Core
import Eventuria.GSD.Write.Flow.Sourcer.Client.Settings

data Dependencies = Dependencies {logger :: Logger ,
                    url :: BaseUrl,
                    httpClientManager :: Manager}

getDependencies :: Settings -> IO(Dependencies)
getDependencies Settings { url = URL {host,port,path}, loggerId} = do
  logger <- getLogger loggerId
  httpClientManager <- (newManager defaultManagerSettings)
  return Dependencies {url = BaseUrl Http host port path ,..}