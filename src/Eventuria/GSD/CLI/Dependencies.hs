{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Eventuria.GSD.CLI.Dependencies where


import Eventuria.Commons.Logger.Core
import qualified Eventuria.GSD.Monitoring.API.Client.Dependencies as Monitoring.Client
import qualified Eventuria.GSD.Monitoring.API.Client.Client as Monitoring.Client

import qualified Eventuria.GSD.Read.API.Client.Client as Read.Client
import qualified Eventuria.GSD.Read.API.Client.Dependencies as Read.Client

import qualified Eventuria.GSD.Write.CommandSourcer.Client.Client as CommandSourcer.Client
import qualified Eventuria.GSD.Write.CommandSourcer.Client.Dependencies as CommandSourcer.Client

import qualified Eventuria.GSD.Write.CommandConsumer.API.HealthCheck.Client.Client as Command.Consumer.Client
import qualified Eventuria.GSD.Write.CommandConsumer.API.HealthCheck.Client.Dependencies as Command.Consumer.Client

import Eventuria.Commons.DevOps.Core
import Eventuria.GSD.CLI.Settings
import Data.Validation
import Control.Lens
import Data.List.NonEmpty
import Eventuria.Commons.Dependencies.Core

data Dependencies = Dependencies { logger :: Logger,
                                   clientDependencies :: ClientDependencies}

data ClientDependencies = ClientDependencies { commandSourcer :: CommandSourcer.Client.Dependencies,
                                               commandConsumer :: Command.Consumer.Client.Dependencies,
                                               read :: Read.Client.Dependencies,
                                               monitoring :: Monitoring.Client.Dependencies}


retrieveDependencies :: Settings -> IO(Validation (NonEmpty UnhealthyDependency) Dependencies)
retrieveDependencies Settings {loggerId,
                   commandSourcerClientSettings,
                   commandConsumerClientSettings,
                   readClientSettings,
                   monitoringClientSettings} = do
                   
  logger          <- getLogger loggerId
  commandSourcer  <- CommandSourcer.Client.getDependencies   commandSourcerClientSettings
  commandConsumer <- Command.Consumer.Client.getDependencies commandConsumerClientSettings
  read            <- Read.Client.getDependencies             readClientSettings
  monitoring      <- Monitoring.Client.getDependencies       monitoringClientSettings

  commandSourcerHealth  <- CommandSourcer.Client.healthCheck   commandSourcer  <&> (toAccValidation $ unhealthyDependency "Command Sourcer")
  commandConsumerHealth <- Command.Consumer.Client.healthCheck commandConsumer <&> (toAccValidation $ unhealthyDependency "Command Consumer")
  readHealth            <- Read.Client.healthCheck             read            <&> (toAccValidation $ unhealthyDependency "Read Projection")
  monitoringHealth      <- Monitoring.Client.healthCheck       monitoring      <&> (toAccValidation $ unhealthyDependency "Monitoring Service")
                                      

  return $ pure Dependencies { clientDependencies = ClientDependencies {..}, ..} <*
           commandSourcerHealth   <*
           commandConsumerHealth  <*
           readHealth             <*
           monitoringHealth

  where

    toAccValidation :: (UnhealthyReason -> UnhealthyDependency) ->
                        HealthCheckResult ->
                        Validation (NonEmpty UnhealthyDependency) ()
    toAccValidation errorHandler = either
                        (\unhealthyReason -> _Failure # (pure $ errorHandler unhealthyReason))
                        (\healthy -> _Success # healthy )

    unhealthyDependency :: String -> UnhealthyReason -> UnhealthyDependency
    unhealthyDependency = (\name unhealthyReason -> UnhealthyDependency {..})
    