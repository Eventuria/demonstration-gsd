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

streamWorkspaceId :: IsStream stream => S.ClientM (stream IO (Persisted WorkspaceId) )
streamWorkspaceId = fromPipes <$> streamWorkspaceIdOnPipe

streamGsdCommandByWorkspaceId :: IsStream stream => WorkspaceId -> S.ClientM (stream IO (Persisted GsdCommand) )
streamGsdCommandByWorkspaceId workspaceId = fromPipes <$> (streamGsdCommandByWorkspaceIdOnPipe workspaceId)


streamInfinitelyGsdCommandByWorkspaceId :: IsStream stream => WorkspaceId -> S.ClientM (stream IO (Persisted GsdCommand) )
streamInfinitelyGsdCommandByWorkspaceId workspaceId = fromPipes <$> (streamInfinitelyGsdCommandByWorkspaceIdOnPipe workspaceId)

streamGsdCommandResponseByWorkspaceId :: IsStream stream => WorkspaceId -> S.ClientM (stream IO (Persisted CommandResponse) )
streamGsdCommandResponseByWorkspaceId workspaceId = fromPipes <$> (streamGsdCommandResponseByWorkspaceIdOnPipe workspaceId)

streamGsdEventByWorkspaceId :: IsStream stream => WorkspaceId -> S.ClientM (stream IO (Persisted GsdEvent) )
streamGsdEventByWorkspaceId workspaceId = fromPipes <$> (streamGsdEventByWorkspaceIdOnPipe workspaceId)

streamInfinitelyGsdEventByWorkspaceId :: IsStream stream => WorkspaceId -> S.ClientM (stream IO (Persisted GsdEvent) )
streamInfinitelyGsdEventByWorkspaceId workspaceId = fromPipes <$> (streamInfinitelyGsdEventByWorkspaceIdOnPipe workspaceId)

streamGsdValidationStateByWorkspaceId :: IsStream stream => WorkspaceId -> S.ClientM (stream IO (Persisted (ValidationState GsdState)) )
streamGsdValidationStateByWorkspaceId workspaceId = fromPipes <$> (streamGsdValidationStateByWorkspaceIdOnPipe workspaceId)

gsdMonitoringApi :: Proxy GSDMonitoringStreamingApi
gsdMonitoringApi = Proxy

streamWorkspaceIdOnPipe :: S.ClientM (P.Producer (Persisted WorkspaceId) IO () )
streamGsdCommandByWorkspaceIdOnPipe :: WorkspaceId -> S.ClientM (P.Producer (Persisted GsdCommand) IO () )
streamInfinitelyGsdCommandByWorkspaceIdOnPipe :: WorkspaceId -> S.ClientM (P.Producer (Persisted GsdCommand) IO ())
streamGsdCommandResponseByWorkspaceIdOnPipe :: WorkspaceId -> S.ClientM (P.Producer (Persisted CommandResponse) IO ())
streamGsdEventByWorkspaceIdOnPipe :: WorkspaceId -> S.ClientM (P.Producer (Persisted GsdEvent) IO ())
streamInfinitelyGsdEventByWorkspaceIdOnPipe :: WorkspaceId -> S.ClientM (P.Producer (Persisted GsdEvent) IO ())
streamGsdValidationStateByWorkspaceIdOnPipe :: WorkspaceId -> S.ClientM (P.Producer (Persisted (ValidationState GsdState)) IO ())
streamWorkspaceIdOnPipe
  :<|> streamGsdCommandByWorkspaceIdOnPipe
  :<|> streamInfinitelyGsdCommandByWorkspaceIdOnPipe
  :<|> streamGsdCommandResponseByWorkspaceIdOnPipe
  :<|> streamGsdEventByWorkspaceIdOnPipe
  :<|> streamInfinitelyGsdEventByWorkspaceIdOnPipe
  :<|> streamGsdValidationStateByWorkspaceIdOnPipe = S.client gsdMonitoringApi

