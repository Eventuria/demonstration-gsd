{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Eventuria.GSD.Monitoring.API.Client.Client (
  healthCheck,
  streamGsdCommandByWorkspaceId,
  streamGsdCommandResponseByWorkspaceId,
  streamGsdEventByWorkspaceId,
  streamGsdWriteModelHistoryByWorkspaceId,
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

import           Eventuria.Commons.Dependencies.Core
import           Eventuria.Commons.Logger.Core

import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import           Eventuria.GSD.Write.Model.Core
import           Eventuria.GSD.Write.Model.Commands.Command
import           Eventuria.GSD.Write.Model.Commands.Serialization ()
import           Eventuria.GSD.Write.Model.Events.Event
import           Eventuria.GSD.Write.Model.Events.Serialization()
import           Eventuria.GSD.Write.Model.WriteModel

import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import           Eventuria.Libraries.CQRS.Write.Serialization.CommandResponse ()

import           Eventuria.GSD.Monitoring.API.Client.Dependencies



data MonitoringServerDown = MonitoringServerDown  deriving Show

instance Exception MonitoringServerDown

streamGsdCommandByWorkspaceId ::           Dependencies -> WorkspaceId -> IO (Either MonitoringServerDown [Persisted GSDCommand])
streamGsdCommandResponseByWorkspaceId ::   Dependencies -> WorkspaceId -> IO (Either MonitoringServerDown [Persisted CommandResponse])
streamGsdEventByWorkspaceId ::             Dependencies -> WorkspaceId -> IO (Either MonitoringServerDown [Persisted GsdEvent])
streamGsdWriteModelHistoryByWorkspaceId :: Dependencies -> WorkspaceId -> IO (Either MonitoringServerDown [Persisted (Maybe GsdWriteModel)])

streamGsdCommandByWorkspaceId =             bindWithSettings streamGsdCommandByWorkspaceIdOnPipe
streamGsdCommandResponseByWorkspaceId  =    bindWithSettings streamGsdCommandResponseByWorkspaceIdOnPipe
streamGsdEventByWorkspaceId  =              bindWithSettings streamGsdEventByWorkspaceIdOnPipe
streamGsdWriteModelHistoryByWorkspaceId  =  bindWithSettings streamGsdWriteModelHistoryByWorkspaceIdOnPipe

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
streamGsdCommandByWorkspaceIdOnPipe ::           WorkspaceId -> S.ClientM (P.Producer (Persisted GSDCommand)                 IO ())
streamGsdCommandResponseByWorkspaceIdOnPipe ::   WorkspaceId -> S.ClientM (P.Producer (Persisted CommandResponse)            IO ())
streamGsdEventByWorkspaceIdOnPipe ::             WorkspaceId -> S.ClientM (P.Producer (Persisted GsdEvent)                   IO ())
streamGsdWriteModelHistoryByWorkspaceIdOnPipe :: WorkspaceId -> S.ClientM (P.Producer (Persisted (Maybe GsdWriteModel)) IO ())
healthCheckCall
  :<|> streamGsdCommandByWorkspaceIdOnPipe
  :<|> streamGsdCommandResponseByWorkspaceIdOnPipe
  :<|> streamGsdEventByWorkspaceIdOnPipe
  :<|> streamGsdWriteModelHistoryByWorkspaceIdOnPipe = S.client gsdMonitoringApi
 where
  gsdMonitoringApi :: Proxy GSDMonitoringStreamingApi
  gsdMonitoringApi = Proxy
