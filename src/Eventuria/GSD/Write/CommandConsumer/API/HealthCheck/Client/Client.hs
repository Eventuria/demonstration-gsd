{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Eventuria.GSD.Write.CommandConsumer.API.HealthCheck.Client.Client where

import Servant
import Eventuria.Adapters.Servant.Wrapper
import           Control.Exception
import Eventuria.GSD.Write.CommandConsumer.API.HealthCheck.Client.Dependencies
import qualified Servant.Client.Streaming as S
import Eventuria.GSD.Write.CommandConsumer.API.HealthCheck.Definition
import Eventuria.GSD.Write.Model.Commands.Serialization ()
import Eventuria.Libraries.CQRS.Write.Serialization.CommandResponse ()
import Eventuria.Commons.DevOps.Core

data CommandConsumerDown = CommandConsumerDown  deriving Show

instance Exception CommandConsumerDown

healthCheck :: Dependencies -> IO (Either CommandConsumerDown Healthy)
healthCheck    Dependencies { httpClientManager, url, logger}  =
  catch
    (S.withClientM
       healthCheckCall
       (S.mkClientEnv httpClientManager url)
       (\e -> do
          case e of
            Left errorHttpLevel -> return $ Left CommandConsumerDown
            Right healthy  -> return $ Right () ))
    (\SomeException {} -> return $ Left CommandConsumerDown )

  where
    healthCheckCall :: S.ClientM Healthy
    healthCheckCall  = S.client (proxy :: Proxy GsdCommandConsumerApi)




