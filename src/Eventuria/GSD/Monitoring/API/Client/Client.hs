{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Eventuria.GSD.Monitoring.API.Client.Client (
  healthCheck,
  streamGsdCommandByWorkspaceId,
  streamInfinitelyGsdCommandByWorkspaceId,
  streamGsdCommandResponseByWorkspaceId,
  streamGsdEventByWorkspaceId,
  streamInfinitelyGsdEventByWorkspaceId,
  streamGsdValidationStateByWorkspaceId,
  MonitoringServerDown) where

import           Data.Proxy
import           Control.Exception

import qualified Streamly.Prelude as Streamly
import           Servant
import           Servant.Pipes ()
import qualified Pipes as P
import qualified Servant.Client.Streaming as S

import           Eventuria.GSD.Monitoring.API.Definition

import           Eventuria.Adapters.Streamly.Adapters

import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import           Eventuria.GSD.Write.Model.Core
import           Eventuria.GSD.Write.Model.Commands.Command
import           Eventuria.GSD.Write.Model.Commands.Serialization ()
import           Eventuria.GSD.Write.Model.Events.Event
import           Eventuria.GSD.Write.Model.Events.Serialization()
import           Eventuria.GSD.Write.Model.State
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import           Eventuria.Libraries.CQRS.Write.Serialization.ValidationState ()
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import           Eventuria.Libraries.CQRS.Write.Serialization.CommandResponse ()
import           Eventuria.Commons.DevOps.Core

import           Eventuria.GSD.Monitoring.API.Client.Dependencies
import           Eventuria.Commons.Logger.Core



data MonitoringServerDown = MonitoringServerDown  deriving Show

instance Exception MonitoringServerDown

streamGsdCommandByWorkspaceId ::           Dependencies -> WorkspaceId -> IO (Either MonitoringServerDown [Persisted GsdCommand])
streamInfinitelyGsdCommandByWorkspaceId :: Dependencies -> WorkspaceId -> IO (Either MonitoringServerDown [Persisted GsdCommand])
streamGsdCommandResponseByWorkspaceId ::   Dependencies -> WorkspaceId -> IO (Either MonitoringServerDown [Persisted CommandResponse])
streamGsdEventByWorkspaceId ::             Dependencies -> WorkspaceId -> IO (Either MonitoringServerDown [Persisted GsdEvent])
streamInfinitelyGsdEventByWorkspaceId ::   Dependencies -> WorkspaceId -> IO (Either MonitoringServerDown [Persisted GsdEvent])
streamGsdValidationStateByWorkspaceId ::   Dependencies -> WorkspaceId -> IO (Either MonitoringServerDown [Persisted (ValidationState GsdState)])

streamGsdCommandByWorkspaceId =             bindWithSettings streamGsdCommandByWorkspaceIdOnPipe
streamInfinitelyGsdCommandByWorkspaceId  =  bindWithSettings streamInfinitelyGsdCommandByWorkspaceIdOnPipe
streamGsdCommandResponseByWorkspaceId  =    bindWithSettings streamGsdCommandResponseByWorkspaceIdOnPipe
streamGsdEventByWorkspaceId  =              bindWithSettings streamGsdEventByWorkspaceIdOnPipe
streamInfinitelyGsdEventByWorkspaceId  =    bindWithSettings streamInfinitelyGsdEventByWorkspaceIdOnPipe
streamGsdValidationStateByWorkspaceId  =    bindWithSettings streamGsdValidationStateByWorkspaceIdOnPipe

healthCheck :: Dependencies -> IO (Either MonitoringServerDown Healthy)
healthCheck    Dependencies { httpClientManager, url, logger}  =
  catch
    (S.withClientM
       healthCheckCall
       (S.mkClientEnv httpClientManager url)
       (\e -> do
          case e of
            Left errorHttpLevel -> return $ Left MonitoringServerDown
            Right healthy  -> return $ Right () ))
    (\SomeException {} -> return $ Left MonitoringServerDown )

bindWithSettings :: (WorkspaceId -> S.ClientM (P.Producer (Persisted item) IO ())) ->
                    Dependencies ->
                    WorkspaceId ->
                    IO (Either MonitoringServerDown [Persisted item])
bindWithSettings call Dependencies { httpClientManager, url, logger} workspaceId =
  catch
    (S.withClientM
       (fromPipes <$> (call workspaceId) )
       (S.mkClientEnv httpClientManager url)
       (\e -> case e of
          Left errorHttpLevel -> do
           logInfo logger "An http error occured with the monitoring microservice."
           return $ Left $ MonitoringServerDown
          Right stream -> do
           list <- Streamly.toList stream
           return $ Right list))
    (\SomeException {} -> return $ Left $ MonitoringServerDown )




healthCheckCall :: S.ClientM Healthy
streamGsdCommandByWorkspaceIdOnPipe ::           WorkspaceId -> S.ClientM (P.Producer (Persisted GsdCommand)                 IO ())
streamInfinitelyGsdCommandByWorkspaceIdOnPipe :: WorkspaceId -> S.ClientM (P.Producer (Persisted GsdCommand)                 IO ())
streamGsdCommandResponseByWorkspaceIdOnPipe ::   WorkspaceId -> S.ClientM (P.Producer (Persisted CommandResponse)            IO ())
streamGsdEventByWorkspaceIdOnPipe ::             WorkspaceId -> S.ClientM (P.Producer (Persisted GsdEvent)                   IO ())
streamInfinitelyGsdEventByWorkspaceIdOnPipe ::   WorkspaceId -> S.ClientM (P.Producer (Persisted GsdEvent)                   IO ())
streamGsdValidationStateByWorkspaceIdOnPipe ::   WorkspaceId -> S.ClientM (P.Producer (Persisted (ValidationState GsdState)) IO ())
healthCheckCall
  :<|> streamGsdCommandByWorkspaceIdOnPipe
  :<|> streamInfinitelyGsdCommandByWorkspaceIdOnPipe
  :<|> streamGsdCommandResponseByWorkspaceIdOnPipe
  :<|> streamGsdEventByWorkspaceIdOnPipe
  :<|> streamInfinitelyGsdEventByWorkspaceIdOnPipe
  :<|> streamGsdValidationStateByWorkspaceIdOnPipe = S.client gsdMonitoringApi
 where
  gsdMonitoringApi :: Proxy GSDMonitoringStreamingApi
  gsdMonitoringApi = Proxy
