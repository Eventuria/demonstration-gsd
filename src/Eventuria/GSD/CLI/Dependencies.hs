{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Eventuria.GSD.CLI.Dependencies where


import Eventuria.Commons.Logger.Core
import qualified Eventuria.GSD.Read.API.Client.Dependencies as Read.Client
import qualified Eventuria.GSD.Write.Flow.Sourcer.Client.Dependencies as Write.Client
import qualified Eventuria.GSD.Write.Flow.CommandConsumer.API.HealthCheck.Client.Dependencies as Write.Command.Consumer.Client
import qualified Eventuria.GSD.Monitoring.API.Client.Dependencies as Monitoring.Client
import qualified Eventuria.GSD.Monitoring.API.Client.Client as Monitoring.Client
import qualified Eventuria.GSD.Read.API.Client.Client as Read.Client
import qualified Eventuria.GSD.Write.Flow.Sourcer.Client.Client as Write.Client
import qualified Eventuria.GSD.Write.Flow.CommandConsumer.API.HealthCheck.Client.Client as Write.Command.Consumer.Client

import Eventuria.Commons.DevOps.Core
import Eventuria.GSD.CLI.Settings
import Data.Validation
import Control.Lens
import Data.List.NonEmpty
import Eventuria.Commons.Dependencies.Core

data Dependencies = Dependencies { logger :: Logger,
                                   writeClientDependencies :: Write.Client.Dependencies,
                                   writeCommandConsumerClientDependencies :: Write.Command.Consumer.Client.Dependencies,
                                   readClientDependencies :: Read.Client.Dependencies,
                                   monitoringClientDependencies :: Monitoring.Client.Dependencies}


retrieveDependencies :: Settings -> IO(Validation (NonEmpty UnhealthyDependency) Dependencies)
retrieveDependencies Settings {loggerId,
                   writeClientSettings,
                   writeCommandConsumerClientSettings,
                   readClientSettings,
                   monitoringClientSettings} = do
  logger           <- getLogger loggerId
  writeClientDependencies                <- Write.Client.getDependencies                   writeClientSettings
  writeCommandConsumerClientDependencies <- Write.Command.Consumer.Client.getDependencies  writeCommandConsumerClientSettings
  readClientDependencies                 <- Read.Client.getDependencies                    readClientSettings
  monitoringClientDependencies           <- Monitoring.Client.getDependencies              monitoringClientSettings

  writeHealth <- Write.Client.healthCheck writeClientDependencies
                    <&> toAccValidation (\unhealthyReason -> UnhealthyDependency {name = "Write / Command Sourcer", ..})
  writeCommandConsumerHealth <- Write.Command.Consumer.Client.healthCheck writeCommandConsumerClientDependencies
                    <&> toAccValidation (\unhealthyReason -> UnhealthyDependency {name = "Write / Command Consumer (Command -> Events)", ..})
  readHealth <- Read.Client.healthCheck readClientDependencies
                    <&> toAccValidation (\unhealthyReason ->   UnhealthyDependency {name = "Read Projection", ..})
  monitoringHealth  <- Monitoring.Client.healthCheck monitoringClientDependencies
                    <&> toAccValidation (\unhealthyReason ->   UnhealthyDependency {name = "Monitoring Service", ..})

  return $ pure Dependencies {..}     <*
           writeHealth                <*
           writeCommandConsumerHealth <*
           readHealth                 <*
           monitoringHealth

  where

    toAccValidation :: (UnhealthyReason -> UnhealthyDependency) -> HealthCheckResult -> Validation (NonEmpty UnhealthyDependency) ()
    toAccValidation errorHandler = either
                        (\unhealthyReason -> _Failure # (pure $ errorHandler unhealthyReason))
                        (\healthy -> _Success # healthy )