{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Eventuria.GSD.CLI.Dependencies where

import           Data.Validation
import           Data.List.NonEmpty
                 
import           Control.Lens

import           Eventuria.Commons.Logger.Core
import qualified Eventuria.GSD.Monitoring.API.Client.Dependencies                        as Monitoring.Client
import qualified Eventuria.GSD.Monitoring.API.Client.Client                              as Monitoring.Client

import qualified Eventuria.GSD.Read.API.Client.Client                                    as Read.Client
import qualified Eventuria.GSD.Read.API.Client.Dependencies                              as Read.Client

import qualified Eventuria.GSD.Write.CommandSourcer.Client.Client                        as Command.Sourcer.Client
import qualified Eventuria.GSD.Write.CommandSourcer.Client.Dependencies                  as Command.Sourcer.Client

import qualified Eventuria.GSD.Write.CommandConsumer.API.HealthCheck.Client.Client       as Command.Consumer.Client
import qualified Eventuria.GSD.Write.CommandConsumer.API.HealthCheck.Client.Dependencies as Command.Consumer.Client

import           Eventuria.Commons.Dependencies.Core
import           Eventuria.GSD.CLI.Settings





data Dependencies = Dependencies { logger :: Logger,
                                   clientDependencies :: ClientDependencies}

data ClientDependencies = ClientDependencies { commandSourcer :: Command.Sourcer.Client.Dependencies,
                                               commandConsumer :: Command.Consumer.Client.Dependencies,
                                               read :: Read.Client.Dependencies,
                                               monitoring :: Monitoring.Client.Dependencies}


getDependencies :: Settings -> IO(Dependencies)
getDependencies    Settings {loggerId,
                             clientSettings = ClientSettings {
                               commandSourcer,
                               commandConsumer,
                               read,
                               monitoring}} = do
                   
  logger          <- getLogger loggerId
  commandSourcer  <- Command.Sourcer.Client.getDependencies  commandSourcer  
  commandConsumer <- Command.Consumer.Client.getDependencies commandConsumer 
  read            <- Read.Client.getDependencies             read            
  monitoring      <- Monitoring.Client.getDependencies       monitoring      

  return Dependencies { clientDependencies = ClientDependencies {..}, ..}
  
checkDependenciesHealth :: Dependencies -> IO(Validation (NonEmpty UnhealthyDependency) Healthy)
checkDependenciesHealth dependencies @ Dependencies {clientDependencies = ClientDependencies {..} } = do
  commandSourcerHealth  <- Command.Sourcer.Client.healthCheck  commandSourcer   <&> (toAccValidation $ unhealthyDependency "Command Sourcer")
  commandConsumerHealth <- Command.Consumer.Client.healthCheck commandConsumer  <&> (toAccValidation $ unhealthyDependency "Command Consumer")
  readHealth            <- Read.Client.healthCheck             read             <&> (toAccValidation $ unhealthyDependency "Read Projection")
  monitoringHealth      <- Monitoring.Client.healthCheck       monitoring       <&> (toAccValidation $ unhealthyDependency "Monitoring Service")
                                      

  return $ pure ()                <*
           commandSourcerHealth   <*
           commandConsumerHealth  <*
           readHealth             <*
           monitoringHealth

  where

    toAccValidation :: Show exception =>  (UnhealthyReason -> UnhealthyDependency) ->
                        Either exception Healthy ->
                        Validation (NonEmpty UnhealthyDependency) ()
    toAccValidation errorHandler = either
                        (\someException -> _Failure # (pure $ errorHandler $ show someException))
                        (\healthy -> _Success # healthy )

    unhealthyDependency :: String -> UnhealthyReason -> UnhealthyDependency
    unhealthyDependency = (\name unhealthyReason -> UnhealthyDependency {..})

