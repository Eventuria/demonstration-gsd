{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module DevOps.MicroService.MonitoringApi where


import Servant.Client (Scheme)
import Control.Exception
import DevOps.Core

data MonitoringApiMicroService = MonitoringApiMicroService {
                                    scheme :: Scheme,
                                    urlHost :: String,
                                    port :: Int}

instance MicroService MonitoringApiMicroService where

  healthCheck service = do
     catch connect (\e @SomeException {} -> return $ Unhealthy "connection to the microservice monitoring-api failed")
    where
      connect :: IO HealthCheckResult
      connect = return Healthy

