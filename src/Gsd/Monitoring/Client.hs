{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Gsd.Monitoring.Client (
  streamGsdCommandByWorkspaceId,
  streamInfinitelyGsdCommandByWorkspaceId,
  streamGsdCommandResponseByWorkspaceId,
  streamGsdEventByWorkspaceId,
  streamInfinitelyGsdEventByWorkspaceId,
  streamGsdValidationStateByWorkspaceId) where

import Data.Proxy
import Gsd.Monitoring.WebStreamingApiDefinition

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

streamGsdCommandByWorkspaceId :: ClientSetting ->
                                 WorkspaceId ->
                                 IO (SafeResponse [Persisted GsdCommand])
streamGsdCommandByWorkspaceId clientSetting workspaceId = do
  bindWithSettings
    clientSetting
    streamGsdCommandByWorkspaceIdOnPipe
    workspaceId

streamInfinitelyGsdCommandByWorkspaceId :: ClientSetting ->
                                           WorkspaceId ->
                                           IO (SafeResponse [Persisted GsdCommand])
streamInfinitelyGsdCommandByWorkspaceId clientSetting workspaceId =
  bindWithSettings
    clientSetting
    streamInfinitelyGsdCommandByWorkspaceIdOnPipe
    workspaceId

streamGsdCommandResponseByWorkspaceId :: ClientSetting ->
                                         WorkspaceId ->
                                         IO (SafeResponse [Persisted CommandResponse])
streamGsdCommandResponseByWorkspaceId clientSetting workspaceId =
  bindWithSettings
    clientSetting
    streamGsdCommandResponseByWorkspaceIdOnPipe
    workspaceId


streamGsdEventByWorkspaceId :: ClientSetting ->
                                WorkspaceId ->
                                IO (SafeResponse [Persisted GsdEvent])
streamGsdEventByWorkspaceId clientSetting workspaceId =
  bindWithSettings
    clientSetting
    streamGsdEventByWorkspaceIdOnPipe
    workspaceId


streamInfinitelyGsdEventByWorkspaceId ::  ClientSetting ->
                                          WorkspaceId ->
                                          IO (SafeResponse [Persisted GsdEvent])
streamInfinitelyGsdEventByWorkspaceId clientSetting workspaceId =
  bindWithSettings
    clientSetting
    streamInfinitelyGsdEventByWorkspaceIdOnPipe
    workspaceId

streamGsdValidationStateByWorkspaceId :: ClientSetting ->
                                         WorkspaceId ->
                                         IO (SafeResponse [Persisted (ValidationState GsdState)])
streamGsdValidationStateByWorkspaceId clientSetting workspaceId =
  bindWithSettings
    clientSetting
    streamGsdValidationStateByWorkspaceIdOnPipe
    workspaceId

bindWithSettings :: ClientSetting ->
                    (WorkspaceId -> S.ClientM (P.Producer (SafeResponse (Persisted item)) IO ())) ->
                    WorkspaceId ->
                    IO (SafeResponse [Persisted item])
bindWithSettings ClientSetting { manager, url, logger} call workspaceId = do
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



gsdMonitoringApi :: Proxy GSDMonitoringStreamingApi
gsdMonitoringApi = Proxy


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

