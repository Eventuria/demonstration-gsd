{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.GSD.Write.CommandConsumer.API.HealthCheck.Server where

import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Exception hiding (Handler)
import           Control.Concurrent

import           Servant
import           Network.Wai.Handler.Warp hiding (Settings)
                 
import           Eventuria.Commons.Dependencies.Core
import           Eventuria.Commons.Logger.Core
import           Eventuria.Commons.Network.Core
                 
import           Eventuria.Adapters.Servant.Wrapper
                 
import           Eventuria.GSD.Write.CommandConsumer.API.HealthCheck.Definition
import qualified Eventuria.GSD.Write.CommandConsumer.Dependencies as Consumer


runServerOnWarp :: Consumer.Dependencies -> IO()
runServerOnWarp dependencies @ Consumer.Dependencies {logger,port} = do
 logInfo logger "HealthCheck Server Up and Running"
 serverThreadId <- myThreadId
 run port $ application
              (proxy :: Proxy GsdCommandConsumerApi)
              (healthCheckServer serverThreadId)
              dependencies
 where
  healthCheckServer :: ServerThreadId -> ServantServer GsdCommandConsumerApi Consumer.Dependencies
  healthCheckServer serverThreadId dependencies = healthCheck serverThreadId dependencies
    where
      healthCheck :: ServerThreadId -> Consumer.Dependencies -> Handler Healthy
      healthCheck serverThreadId Consumer.Dependencies {logger} =
        liftIO $ logInfo logger "service health asked"  >>
                 Consumer.healthCheck dependencies >>=
                 either
                   (\error -> do
                       logInfo logger $ "service unhealthy : " ++ show error
                       return $ Left $ toException ServerDownException)
                   (\right -> do
                       logInfo logger "service healthy"
                       return $ Right ()) >>=
                 breakServerOnFailure logger serverThreadId