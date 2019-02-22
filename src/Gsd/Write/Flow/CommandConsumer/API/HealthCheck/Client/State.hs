{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Write.Flow.CommandConsumer.API.HealthCheck.Client.State where

import Servant.Client
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Eventuria.Commons.Logger.Core
import Eventuria.Commons.Network.Core
import Gsd.Write.Flow.CommandConsumer.API.HealthCheck.Client.Settings

data State = State {logger :: Logger ,
                    url :: BaseUrl,
                    httpClientManager :: Manager}

getState :: Settings -> IO(State)
getState Settings { url = URL {host,port,path}, loggerId} = do
  logger <- getLogger loggerId
  httpClientManager <- (newManager defaultManagerSettings)
  return State {url = BaseUrl Http host port path ,..}