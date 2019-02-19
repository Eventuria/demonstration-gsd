{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Gsd.CLI.Dependencies where


import Logger.Core
import qualified Gsd.Read.API.Client.State as Read.Client
import qualified Gsd.Write.API.Client.State as Write.Client
import qualified Gsd.Monitoring.API.Client.State as Monitoring.Client
import qualified Gsd.Monitoring.API.Client.Client as Monitoring.Client
import qualified Gsd.Read.API.Client.Client as Read.Client
import qualified Gsd.Write.API.Client.Client as Write.Client

import DevOps.Core
import Gsd.CLI.Settings
import Data.Validation
import Control.Lens
import Data.List.NonEmpty
import Dependencies.Core

data Dependencies = Dependencies { logger :: Logger,
                                   writeClientDependencies :: Write.Client.State,
                                   readClientDependencies :: Read.Client.State,
                                   monitoringClientDependencies :: Monitoring.Client.State}


retrieveDependencies :: Settings -> IO(Validation (NonEmpty UnhealthyDependency) Dependencies)
retrieveDependencies Settings {loggerId,
                   writeClientSettings,
                   readClientSettings,
                   monitoringClientSettings} = do
  logger           <- getLogger loggerId
  writeClientDependencies      <- Write.Client.getState      writeClientSettings
  readClientDependencies       <- Read.Client.getState       readClientSettings
  monitoringClientDependencies <- Monitoring.Client.getState monitoringClientSettings

  writeHealth <- Write.Client.healthCheck writeClientDependencies
                          <&> toAccValidation (\unhealthyReason -> UnhealthyDependency {name = "Write", ..})
  readHealth <- Read.Client.healthCheck readClientDependencies
                        <&> toAccValidation (\unhealthyReason ->   UnhealthyDependency {name = "Read", ..})
  monitoringHealth  <- Monitoring.Client.healthCheck monitoringClientDependencies
                        <&> toAccValidation (\unhealthyReason ->   UnhealthyDependency {name = "Monitoring", ..})

  return $ pure Dependencies {..} <*
           writeHealth            <*
           readHealth             <*
           monitoringHealth

  where

    toAccValidation :: (UnhealthyReason -> UnhealthyDependency) -> HealthCheckResult -> Validation (NonEmpty UnhealthyDependency) ()
    toAccValidation errorHandler = either
                        (\unhealthyReason -> _Failure # (pure $ errorHandler unhealthyReason))
                        (\healthy -> _Success # healthy )