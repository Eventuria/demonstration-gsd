{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Gsd.Monitoring.Client (
  streamWorkspaceId,
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
import Streamly
import Gsd.Write.State
import Cqrs.Write.Aggregate.Commands.ValidationStates.ValidationState
import Cqrs.Write.Serialization.ValidationState ()
import Cqrs.Write.Aggregate.Commands.Responses.CommandResponse
import Cqrs.Write.Serialization.CommandResponse ()
import DevOps.Core
import System.SafeResponse



streamWorkspaceId :: IsStream stream => S.ClientM (stream IO (SafeResponse (Persisted WorkspaceId) ))
streamWorkspaceId = fromPipes <$> streamWorkspaceIdOnPipe

streamGsdCommandByWorkspaceId :: IsStream stream =>
                                   WorkspaceId ->
                                   S.ClientM (stream IO (SafeResponse (Persisted GsdCommand) ))
streamGsdCommandByWorkspaceId workspaceId = fromPipes <$> (streamGsdCommandByWorkspaceIdOnPipe workspaceId)


streamInfinitelyGsdCommandByWorkspaceId :: IsStream stream =>
                                            WorkspaceId -> 
                                            S.ClientM (stream IO (SafeResponse (Persisted GsdCommand)) )
streamInfinitelyGsdCommandByWorkspaceId workspaceId = 
    fromPipes <$> (streamInfinitelyGsdCommandByWorkspaceIdOnPipe workspaceId)

streamGsdCommandResponseByWorkspaceId :: IsStream stream => 
                                          WorkspaceId -> 
                                          S.ClientM (stream IO (SafeResponse (Persisted CommandResponse)) )
streamGsdCommandResponseByWorkspaceId workspaceId =
    fromPipes <$> (streamGsdCommandResponseByWorkspaceIdOnPipe workspaceId)

streamGsdEventByWorkspaceId :: IsStream stream => 
                                WorkspaceId -> 
                                S.ClientM (stream IO (SafeResponse (Persisted GsdEvent)) )
streamGsdEventByWorkspaceId workspaceId = fromPipes <$> (streamGsdEventByWorkspaceIdOnPipe workspaceId)

streamInfinitelyGsdEventByWorkspaceId :: IsStream stream => 
                                          WorkspaceId -> 
                                          S.ClientM (stream IO (SafeResponse (Persisted GsdEvent)) )
streamInfinitelyGsdEventByWorkspaceId workspaceId = 
    fromPipes <$> (streamInfinitelyGsdEventByWorkspaceIdOnPipe workspaceId)

streamGsdValidationStateByWorkspaceId :: IsStream stream => 
                                          WorkspaceId -> 
                                          S.ClientM (stream IO (SafeResponse (Persisted (ValidationState GsdState))) )
streamGsdValidationStateByWorkspaceId workspaceId =
    fromPipes <$> (streamGsdValidationStateByWorkspaceIdOnPipe workspaceId)

gsdMonitoringApi :: Proxy GSDMonitoringStreamingApi
gsdMonitoringApi = Proxy


healthCheck :: S.ClientM HealthCheckResult
streamWorkspaceIdOnPipe ::                                      S.ClientM (P.Producer (SafeResponse (Persisted WorkspaceId))                IO ())
streamGsdCommandByWorkspaceIdOnPipe ::           WorkspaceId -> S.ClientM (P.Producer (SafeResponse (Persisted GsdCommand))                 IO ())
streamInfinitelyGsdCommandByWorkspaceIdOnPipe :: WorkspaceId -> S.ClientM (P.Producer (SafeResponse (Persisted GsdCommand))                 IO ())
streamGsdCommandResponseByWorkspaceIdOnPipe ::   WorkspaceId -> S.ClientM (P.Producer (SafeResponse (Persisted CommandResponse))            IO ())
streamGsdEventByWorkspaceIdOnPipe ::             WorkspaceId -> S.ClientM (P.Producer (SafeResponse (Persisted GsdEvent))                   IO ())
streamInfinitelyGsdEventByWorkspaceIdOnPipe ::   WorkspaceId -> S.ClientM (P.Producer (SafeResponse (Persisted GsdEvent))                   IO ())
streamGsdValidationStateByWorkspaceIdOnPipe ::   WorkspaceId -> S.ClientM (P.Producer (SafeResponse (Persisted (ValidationState GsdState))) IO ())
healthCheck
  :<|> streamWorkspaceIdOnPipe
  :<|> streamGsdCommandByWorkspaceIdOnPipe
  :<|> streamInfinitelyGsdCommandByWorkspaceIdOnPipe
  :<|> streamGsdCommandResponseByWorkspaceIdOnPipe
  :<|> streamGsdEventByWorkspaceIdOnPipe
  :<|> streamInfinitelyGsdEventByWorkspaceIdOnPipe
  :<|> streamGsdValidationStateByWorkspaceIdOnPipe = S.client gsdMonitoringApi

