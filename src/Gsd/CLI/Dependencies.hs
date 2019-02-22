{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Gsd.CLI.Dependencies where


import Eventuria.Commons.Logger.Core
import qualified Gsd.Read.API.Client.State as Read.Client
import qualified Gsd.Write.Flow.Sourcer.Client.State as Write.Client
import qualified Gsd.Write.Flow.CommandConsumer.API.HealthCheck.Client.State as Write.Command.Consumer.Client
import qualified Gsd.Monitoring.API.Client.State as Monitoring.Client
import qualified Gsd.Monitoring.API.Client.Client as Monitoring.Client
import qualified Gsd.Read.API.Client.Client as Read.Client
import qualified Gsd.Write.Flow.Sourcer.Client.Client as Write.Client
import qualified Gsd.Write.Flow.CommandConsumer.API.HealthCheck.Client.Client as Write.Command.Consumer.Client

import Eventuria.Commons.DevOps.Core
import Gsd.CLI.Settings
import Data.Validation
import Control.Lens
import Data.List.NonEmpty
import Eventuria.Commons.Dependencies.Core

data Dependencies = Dependencies { logger :: Logger,
                                   writeClientDependencies :: Write.Client.State,
                                   writeCommandConsumerClientDependencies :: Write.Command.Consumer.Client.State,
                                   readClientDependencies :: Read.Client.State,
                                   monitoringClientDependencies :: Monitoring.Client.State}


retrieveDependencies :: Settings -> IO(Validation (NonEmpty UnhealthyDependency) Dependencies)
retrieveDependencies Settings {loggerId,
                   writeClientSettings,
                   writeCommandConsumerClientSettings,
                   readClientSettings,
                   monitoringClientSettings} = do
  logger           <- getLogger loggerId
  writeClientDependencies                <- Write.Client.getState                   writeClientSettings
  writeCommandConsumerClientDependencies <- Write.Command.Consumer.Client.getState  writeCommandConsumerClientSettings
  readClientDependencies                 <- Read.Client.getState                    readClientSettings
  monitoringClientDependencies           <- Monitoring.Client.getState              monitoringClientSettings

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