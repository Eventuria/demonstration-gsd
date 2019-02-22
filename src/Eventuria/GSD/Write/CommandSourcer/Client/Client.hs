{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Eventuria.GSD.Write.CommandSourcer.Client.Client (
  healthCheck,
  sendCommandAndWaitTillProcessed) where

import Servant
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import Eventuria.GSD.Write.Model.Commands.Command
import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId
import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import Eventuria.Libraries.CQRS.Write.PersistCommandResult
import Eventuria.Commons.System.SafeResponse
import Eventuria.GSD.Write.CommandSourcer.Client.Dependencies
import qualified Servant.Client.Streaming as S
import Control.Exception
import Eventuria.Commons.Logger.Core
import Eventuria.GSD.Write.CommandSourcer.Definition
import Eventuria.GSD.Write.Model.Commands.Serialization ()
import Eventuria.Libraries.CQRS.Write.Serialization.CommandResponse ()
import Eventuria.Commons.DevOps.Core
data SendCommandAnWaitFailure =  SendCommandAnWaitFailure {reason :: String} deriving Show

healthCheck :: Dependencies -> IO (HealthCheckResult)
healthCheck Dependencies { httpClientManager, url, logger}  = do
  S.withClientM
     healthCheckCall
     (S.mkClientEnv httpClientManager url)
     (\e -> do
        case e of
          Left errorHttpLevel -> return $ unhealthy $ show errorHttpLevel
          Right healthCheckResult  -> return healthCheckResult )

sendCommandAndWaitTillProcessed :: Dependencies -> GsdCommand -> IO (Either SendCommandAnWaitFailure CommandResponse )
sendCommandAndWaitTillProcessed dependencies @ Dependencies { httpClientManager, url, logger} gsdCommand =
   (S.withClientM
   (sendCommandCall gsdCommand )
   (S.mkClientEnv httpClientManager url)
   (\eitherServantErrorOrResponse -> case eitherServantErrorOrResponse of
      Left servantError -> do
       logInfo logger "An http error occured with the write microservice."
       return $ Left $ SendCommandAnWaitFailure {reason = show servantError }
      Right persistenceResult -> case persistenceResult of
            FailedToPersist {reason } -> return $ Left $ SendCommandAnWaitFailure {reason}
            SuccessfullyPersisted {aggregateId, commandId,lastOffsetPersisted} -> do
              safeResponse <- waitTillCommandResponseProduced
                                dependencies
                                aggregateId
                                lastOffsetPersisted
                                commandId
              case safeResponse of
                Left exception -> return $ Left $ SendCommandAnWaitFailure {reason = show exception }
                Right PersistedItem {item = commandResponse } -> return $ Right commandResponse))

  where

    waitTillCommandResponseProduced :: Dependencies ->
                                         AggregateId ->
                                              Offset ->
                                           CommandId -> IO (SafeResponse (Persisted CommandResponse))
    waitTillCommandResponseProduced Dependencies { httpClientManager, url, logger} aggregateId offset commandId =
      (S.withClientM
       (waitTillCommandResponseProducedCall aggregateId offset commandId)
       (S.mkClientEnv httpClientManager url)
       (\e -> case e of
          Left servantError -> do
           logInfo logger "An http error occured with the write microservice."
           return $ Left $ toException servantError
          Right safeResponse -> return safeResponse))

healthCheckCall :: S.ClientM HealthCheckResult
sendCommandCall :: GsdCommand -> S.ClientM PersistCommandResult
waitTillCommandResponseProducedCall :: AggregateId ->
                                       Offset ->
                                       CommandId ->
                                       S.ClientM (SafeResponse (Persisted CommandResponse))
healthCheckCall
  :<|> sendCommandCall
  :<|> waitTillCommandResponseProducedCall = S.client api
  where
   api :: Proxy GsdWriteApi
   api = Proxy



