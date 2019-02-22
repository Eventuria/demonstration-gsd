{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Gsd.Write.Flow.CommandConsumer.API.HealthCheck.Client.Client where

import Servant
import Eventuria.Adapters.Servant.Wrapper

import Gsd.Write.Flow.CommandConsumer.API.HealthCheck.Client.State
import qualified Servant.Client.Streaming as S
import Gsd.Write.Flow.CommandConsumer.API.HealthCheck.Definition
import Gsd.Write.Model.Commands.Serialization ()
import CQRS.Write.Serialization.CommandResponse ()
import Eventuria.Commons.DevOps.Core


healthCheck :: State -> IO (HealthCheckResult)
healthCheck State { httpClientManager, url, logger}  = do
  S.withClientM
     healthCheckCall
     (S.mkClientEnv httpClientManager url)
     (\e -> do
        case e of
          Left errorHttpLevel -> return $ unhealthy $ show errorHttpLevel
          Right healthCheckResult  -> return healthCheckResult )

  where
    healthCheckCall :: S.ClientM HealthCheckResult
    healthCheckCall  = S.client (proxy :: Proxy GsdCommandConsumerApi)




