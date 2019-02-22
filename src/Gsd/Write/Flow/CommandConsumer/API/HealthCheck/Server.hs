{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.Write.Flow.CommandConsumer.API.HealthCheck.Server where

import Servant
import Servant.Wrapper
import Network.Wai.Handler.Warp hiding (Settings)
import Gsd.Write.Flow.CommandConsumer.API.HealthCheck.Definition
import qualified Gsd.Write.Flow.CommandConsumer.Dependencies as Consumer
import DevOps.Core
import Logger.Core

runServerOnWarp :: Consumer.Dependencies -> IO()
runServerOnWarp dependencies @ Consumer.Dependencies {logger,port} = do
 logInfo logger "HealthCheck Server Started"
 run port $ application
              (proxy :: Proxy GsdCommandConsumerApi)
              healthCheckServer
              dependencies
 where
  healthCheckServer :: ServantServer GsdCommandConsumerApi Consumer.Dependencies
  healthCheckServer dependencies = healthCheck
    where
      healthCheck :: Handler HealthCheckResult
      healthCheck = return healthy