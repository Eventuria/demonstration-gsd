{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Eventuria.GSD.Write.CommandSourcer.Client.Client (
  healthCheck,
  sendCommandAndWaitTillProcessed) where

import           Control.Exception
import           Data.Either.Combinators

import qualified Servant.Client.Streaming as S
import           Servant

import           Eventuria.Commons.DevOps.Core

import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import           Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import           Eventuria.Libraries.CQRS.Write.PersistCommandResult
import           Eventuria.Libraries.CQRS.Write.Serialization.CommandTransaction ()
import           Eventuria.Libraries.CQRS.Write.CommandConsumption.Transaction.CommandTransaction

import           Eventuria.GSD.Write.Model.Commands.Command
import           Eventuria.GSD.Write.CommandSourcer.Client.Dependencies
import           Eventuria.GSD.Write.CommandSourcer.Definition
import           Eventuria.GSD.Write.Model.Commands.Serialization ()


data CommandSourcerServerDown = CommandSourcerServerDown  deriving Show

instance Exception CommandSourcerServerDown

healthCheck :: Dependencies -> IO (Either CommandSourcerServerDown Healthy)
healthCheck    Dependencies { httpClientManager, url, logger}  =
  catch
    (S.withClientM
       healthCheckCall
       (S.mkClientEnv httpClientManager url)
       (\e -> do
          case e of
            Left errorHttpLevel -> return $ Left CommandSourcerServerDown
            Right healthy  -> return $ Right () ))
    (\SomeException {} -> return $ Left CommandSourcerServerDown )

sendCommandAndWaitTillProcessed :: Dependencies -> GsdCommand -> IO (Either CommandSourcerServerDown CommandResponse )
sendCommandAndWaitTillProcessed dependencies @ Dependencies { httpClientManager, url, logger} gsdCommand =
   catch
    (S.withClientM
      (sendCommandCall gsdCommand )
      (S.mkClientEnv httpClientManager url)
      (\eitherServantErrorOrResponse -> case eitherServantErrorOrResponse of
          Left servantError -> return $ Left CommandSourcerServerDown
          Right (PersistCommandResult {aggregateId, commandId,lastOffsetPersisted}) -> do
                  result <- waitTillCommandResponseProduced
                                    dependencies
                                    aggregateId
                                    lastOffsetPersisted
                                    commandId
                  return $ mapRight toCommandResponse result ))
    (\SomeException {} -> return $ Left $ CommandSourcerServerDown )

  where

    waitTillCommandResponseProduced :: Dependencies ->
                                         AggregateId ->
                                              Offset ->
                                           CommandId -> IO (Either CommandSourcerServerDown CommandTransaction)
    waitTillCommandResponseProduced Dependencies { httpClientManager, url, logger} aggregateId offset commandId =
      (S.withClientM
       (waitTillCommandResponseProducedCall aggregateId offset commandId)
       (S.mkClientEnv httpClientManager url)
       (\e -> case e of
          Left servantError -> return $ Left CommandSourcerServerDown
          Right PersistedItem {item} -> return $ Right item))

healthCheckCall :: S.ClientM Healthy
sendCommandCall :: GsdCommand -> S.ClientM PersistCommandResult
waitTillCommandResponseProducedCall :: AggregateId ->
                                       Offset ->
                                       CommandId ->
                                       S.ClientM (Persisted CommandTransaction)
healthCheckCall
  :<|> sendCommandCall
  :<|> waitTillCommandResponseProducedCall = S.client api
  where
   api :: Proxy GsdWriteApi
   api = Proxy



