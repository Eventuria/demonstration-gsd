{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.GSD.Write.Flow.CommandConsumer.API.HealthCheck.Server where

import Servant
import Eventuria.Adapters.Servant.Wrapper
import Network.Wai.Handler.Warp hiding (Settings)
import Eventuria.GSD.Write.Flow.CommandConsumer.API.HealthCheck.Definition
import qualified Eventuria.GSD.Write.Flow.CommandConsumer.Dependencies as Consumer
import Eventuria.Commons.DevOps.Core
import Eventuria.Commons.Logger.Core

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