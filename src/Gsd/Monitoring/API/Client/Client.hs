{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Gsd.Monitoring.API.Client.Client (
  streamGsdCommandByWorkspaceId,
  streamInfinitelyGsdCommandByWorkspaceId,
  streamGsdCommandResponseByWorkspaceId,
  streamGsdEventByWorkspaceId,
  streamInfinitelyGsdEventByWorkspaceId,
  streamGsdValidationStateByWorkspaceId) where

import Data.Proxy
import Gsd.Monitoring.API.Definition

import Servant
import Streamly.Adapters
import qualified Pipes as P

import qualified Servant.Client.Streaming as S

import PersistedStreamEngine.Interface.PersistedItem
import Gsd.Write.Core
import Gsd.Write.Commands.Command
import Gsd.Write.Commands.Serialization ()
import Gsd.Write.Events.Event
import Gsd.Write.Events.Serialization()
import Servant.Pipes ()
import Gsd.Write.State
import Cqrs.Write.Aggregate.Commands.ValidationStates.ValidationState
import Cqrs.Write.Serialization.ValidationState ()
import Cqrs.Write.Aggregate.Commands.Responses.CommandResponse
import Cqrs.Write.Serialization.CommandResponse ()
import DevOps.Core
import System.SafeResponse
import Control.Exception
import Gsd.Clients
import Logger.Core
import qualified Streamly.Safe as StreamlySafe

streamGsdCommandByWorkspaceId ::           ClientSetting -> WorkspaceId -> IO (SafeResponse [Persisted GsdCommand])
streamInfinitelyGsdCommandByWorkspaceId :: ClientSetting -> WorkspaceId -> IO (SafeResponse [Persisted GsdCommand])
streamGsdCommandResponseByWorkspaceId ::   ClientSetting -> WorkspaceId -> IO (SafeResponse [Persisted CommandResponse])
streamGsdEventByWorkspaceId ::             ClientSetting -> WorkspaceId -> IO (SafeResponse [Persisted GsdEvent])
streamInfinitelyGsdEventByWorkspaceId ::   ClientSetting -> WorkspaceId -> IO (SafeResponse [Persisted GsdEvent])
streamGsdValidationStateByWorkspaceId ::   ClientSetting -> WorkspaceId -> IO (SafeResponse [Persisted (ValidationState GsdState)])

streamGsdCommandByWorkspaceId =             bindWithSettings streamGsdCommandByWorkspaceIdOnPipe
streamInfinitelyGsdCommandByWorkspaceId  =  bindWithSettings streamInfinitelyGsdCommandByWorkspaceIdOnPipe
streamGsdCommandResponseByWorkspaceId  =    bindWithSettings streamGsdCommandResponseByWorkspaceIdOnPipe
streamGsdEventByWorkspaceId  =              bindWithSettings streamGsdEventByWorkspaceIdOnPipe
streamInfinitelyGsdEventByWorkspaceId  =    bindWithSettings streamInfinitelyGsdEventByWorkspaceIdOnPipe
streamGsdValidationStateByWorkspaceId  =    bindWithSettings streamGsdValidationStateByWorkspaceIdOnPipe


bindWithSettings :: (WorkspaceId -> S.ClientM (P.Producer (SafeResponse (Persisted item)) IO ())) ->
                    ClientSetting ->
                    WorkspaceId ->
                    IO (SafeResponse [Persisted item])
bindWithSettings call ClientSetting { manager, url, logger} workspaceId = do
  (S.withClientM
     (fromPipes <$> (call workspaceId))
     (S.mkClientEnv manager url)
     (\e -> case e of
        Left errorHttpLevel -> do
         logInfo logger "An http error occured with the monitoring microservice."
         return $ Left $ toException errorHttpLevel
        Right stream -> do
         safeResponse <- StreamlySafe.toList stream
         return safeResponse))


healthCheck :: S.ClientM HealthCheckResult
streamGsdCommandByWorkspaceIdOnPipe ::           WorkspaceId -> S.ClientM (P.Producer (SafeResponse (Persisted GsdCommand))                 IO ())
streamInfinitelyGsdCommandByWorkspaceIdOnPipe :: WorkspaceId -> S.ClientM (P.Producer (SafeResponse (Persisted GsdCommand))                 IO ())
streamGsdCommandResponseByWorkspaceIdOnPipe ::   WorkspaceId -> S.ClientM (P.Producer (SafeResponse (Persisted CommandResponse))            IO ())
streamGsdEventByWorkspaceIdOnPipe ::             WorkspaceId -> S.ClientM (P.Producer (SafeResponse (Persisted GsdEvent))                   IO ())
streamInfinitelyGsdEventByWorkspaceIdOnPipe ::   WorkspaceId -> S.ClientM (P.Producer (SafeResponse (Persisted GsdEvent))                   IO ())
streamGsdValidationStateByWorkspaceIdOnPipe ::   WorkspaceId -> S.ClientM (P.Producer (SafeResponse (Persisted (ValidationState GsdState))) IO ())
healthCheck
  :<|> streamGsdCommandByWorkspaceIdOnPipe
  :<|> streamInfinitelyGsdCommandByWorkspaceIdOnPipe
  :<|> streamGsdCommandResponseByWorkspaceIdOnPipe
  :<|> streamGsdEventByWorkspaceIdOnPipe
  :<|> streamInfinitelyGsdEventByWorkspaceIdOnPipe
  :<|> streamGsdValidationStateByWorkspaceIdOnPipe = S.client gsdMonitoringApi
 where
  gsdMonitoringApi :: Proxy GSDMonitoringStreamingApi
  gsdMonitoringApi = Proxy
