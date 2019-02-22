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
  streamGsdValidationStateByWorkspaceId) where

import Data.Proxy
import Eventuria.GSD.Monitoring.API.Definition

import Servant
import Eventuria.Adapters.Streamly.Adapters
import qualified Pipes as P

import qualified Servant.Client.Streaming as S

import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import Eventuria.GSD.Write.Model.Core
import Eventuria.GSD.Write.Model.Commands.Command
import Eventuria.GSD.Write.Model.Commands.Serialization ()
import Eventuria.GSD.Write.Model.Events.Event
import Eventuria.GSD.Write.Model.Events.Serialization()
import Servant.Pipes ()
import Eventuria.GSD.Write.Model.State
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import Eventuria.Libraries.CQRS.Write.Serialization.ValidationState ()
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import Eventuria.Libraries.CQRS.Write.Serialization.CommandResponse ()
import Eventuria.Commons.DevOps.Core
import Eventuria.Commons.System.SafeResponse
import Control.Exception
import Eventuria.GSD.Monitoring.API.Client.State
import Eventuria.Commons.Logger.Core
import qualified Eventuria.Adapters.Streamly.Safe as StreamlySafe

streamGsdCommandByWorkspaceId ::           State -> WorkspaceId -> IO (SafeResponse [Persisted GsdCommand])
streamInfinitelyGsdCommandByWorkspaceId :: State -> WorkspaceId -> IO (SafeResponse [Persisted GsdCommand])
streamGsdCommandResponseByWorkspaceId ::   State -> WorkspaceId -> IO (SafeResponse [Persisted CommandResponse])
streamGsdEventByWorkspaceId ::             State -> WorkspaceId -> IO (SafeResponse [Persisted GsdEvent])
streamInfinitelyGsdEventByWorkspaceId ::   State -> WorkspaceId -> IO (SafeResponse [Persisted GsdEvent])
streamGsdValidationStateByWorkspaceId ::   State -> WorkspaceId -> IO (SafeResponse [Persisted (ValidationState GsdState)])

streamGsdCommandByWorkspaceId =             bindWithSettings streamGsdCommandByWorkspaceIdOnPipe
streamInfinitelyGsdCommandByWorkspaceId  =  bindWithSettings streamInfinitelyGsdCommandByWorkspaceIdOnPipe
streamGsdCommandResponseByWorkspaceId  =    bindWithSettings streamGsdCommandResponseByWorkspaceIdOnPipe
streamGsdEventByWorkspaceId  =              bindWithSettings streamGsdEventByWorkspaceIdOnPipe
streamInfinitelyGsdEventByWorkspaceId  =    bindWithSettings streamInfinitelyGsdEventByWorkspaceIdOnPipe
streamGsdValidationStateByWorkspaceId  =    bindWithSettings streamGsdValidationStateByWorkspaceIdOnPipe


bindWithSettings :: (WorkspaceId -> S.ClientM (P.Producer (SafeResponse (Persisted item)) IO ())) ->
                    State ->
                    WorkspaceId ->
                    IO (SafeResponse [Persisted item])
bindWithSettings call State { httpClientManager, url, logger} workspaceId = do
  (S.withClientM
     (fromPipes <$> (call workspaceId))
     (S.mkClientEnv httpClientManager url)
     (\e -> case e of
        Left errorHttpLevel -> do
         logInfo logger "An http error occured with the monitoring microservice."
         return $ Left $ toException errorHttpLevel
        Right stream -> do
         safeResponse <- StreamlySafe.toList stream
         return safeResponse))


healthCheck :: State -> IO (HealthCheckResult)
healthCheck State { httpClientManager, url, logger}  = do
  S.withClientM
     healthCheckCall
     (S.mkClientEnv httpClientManager url)
     (\e -> do
        case e of
          Left errorHttpLevel -> return $ unhealthy $ show errorHttpLevel
          Right healthCheckResult  -> return healthCheckResult )


healthCheckCall :: S.ClientM HealthCheckResult
streamGsdCommandByWorkspaceIdOnPipe ::           WorkspaceId -> S.ClientM (P.Producer (SafeResponse (Persisted GsdCommand))                 IO ())
streamInfinitelyGsdCommandByWorkspaceIdOnPipe :: WorkspaceId -> S.ClientM (P.Producer (SafeResponse (Persisted GsdCommand))                 IO ())
streamGsdCommandResponseByWorkspaceIdOnPipe ::   WorkspaceId -> S.ClientM (P.Producer (SafeResponse (Persisted CommandResponse))            IO ())
streamGsdEventByWorkspaceIdOnPipe ::             WorkspaceId -> S.ClientM (P.Producer (SafeResponse (Persisted GsdEvent))                   IO ())
streamInfinitelyGsdEventByWorkspaceIdOnPipe ::   WorkspaceId -> S.ClientM (P.Producer (SafeResponse (Persisted GsdEvent))                   IO ())
streamGsdValidationStateByWorkspaceIdOnPipe ::   WorkspaceId -> S.ClientM (P.Producer (SafeResponse (Persisted (ValidationState GsdState))) IO ())
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
