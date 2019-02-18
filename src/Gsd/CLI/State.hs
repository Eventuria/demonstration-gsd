{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Gsd.CLI.State where


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


data State = State { logger :: Logger,
                     writeClientState :: Write.Client.State,
                     readClientState :: Read.Client.State,
                     monitoringClientState :: Monitoring.Client.State}

type ServiceName = String
data Error = ServiceUnhealthy {serviceName :: ServiceName,
                               unhealthyReason :: UnhealthyReason}



getState :: Settings -> IO(Validation (NonEmpty Error) State)
getState Settings {loggerId,
                   writeClientSettings,
                   readClientSettings,
                   monitoringClientSettings} = do
  logger           <- getLogger loggerId
  writeClientState      <- Write.Client.getState      writeClientSettings
  readClientState       <- Read.Client.getState       readClientSettings
  monitoringClientState <- Monitoring.Client.getState monitoringClientSettings

  writeClientHealth <- (Write.Client.healthCheck writeClientState
                          <&> toAccValidation (\unhealthyReason -> ServiceUnhealthy {serviceName = "Write", ..}))
  readClientHealth <-  (Read.Client.healthCheck readClientState
                        <&> toAccValidation (\unhealthyReason -> ServiceUnhealthy {serviceName = "Read", ..}))
  monitoringHealth  <- (Monitoring.Client.healthCheck monitoringClientState
                        <&> toAccValidation (\unhealthyReason -> ServiceUnhealthy {serviceName = "Monitoring", ..}))

  return $ pure State {..}   <*
           writeClientHealth <*
           readClientHealth  <*
           monitoringHealth

  where

    toAccValidation :: (UnhealthyReason -> Error) -> HealthCheckResult -> Validation (NonEmpty Error) ()
    toAccValidation errorHandler = either
                        (\unhealthyReason -> _Failure # (pure $ errorHandler unhealthyReason))
                        (\healthy -> _Success # healthy )