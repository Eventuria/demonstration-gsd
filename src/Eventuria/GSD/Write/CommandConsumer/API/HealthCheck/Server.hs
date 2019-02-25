{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.GSD.Write.CommandConsumer.API.HealthCheck.Server where

import           Control.Monad.IO.Class (MonadIO(liftIO))

import           Servant
import           Network.Wai.Handler.Warp hiding (Settings)
                 
import           Eventuria.Commons.DevOps.Core
import           Eventuria.Commons.Logger.Core
                 
import           Eventuria.Adapters.Servant.Wrapper
                 
import           Eventuria.GSD.Write.CommandConsumer.API.HealthCheck.Definition
import qualified Eventuria.GSD.Write.CommandConsumer.Dependencies as Consumer
import qualified Eventuria.GSD.Write.CommandConsumer.Settings     as Consumer

runServerOnWarp :: Consumer.Settings -> Consumer.Dependencies -> IO()
runServerOnWarp settings dependencies @ Consumer.Dependencies {logger,port} = do
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
      healthCheck = liftIO $ Consumer.getDependencies
                                settings
                                (\dependencies -> return healthy)
                                (\unhealthyDependencies -> return $ unhealthy "Service unavailable")