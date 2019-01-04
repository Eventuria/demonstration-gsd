{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Gsd.Monitoring.Client (
streamWorkspaceIdsCreated,
streamGsdCommandsByWorkspaceId,
streamInfinitelyGsdCommandsByWorkspaceId,
streamGsdEventsByWorkspaceId,
streamInfinitelyGsdEventsByWorkspaceId) where

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

gsdMonitoringStreamingApi :: Proxy GSDMonitoringStreamingApi
gsdMonitoringStreamingApi = Proxy

streamWorkspaceIdsCreatedOnPipe :: S.ClientM (P.Producer (Persisted WorkspaceId) IO () )
streamGsdCommandsByWorkspaceIdOnPipe :: WorkspaceId -> S.ClientM (P.Producer (Persisted GsdCommand) IO () )
streamInfinitelyGsdCommandsByWorkspaceIdOnPipe :: WorkspaceId -> S.ClientM (P.Producer (Persisted GsdCommand) IO ())
streamGsdEventsByWorkspaceIdOnPipe :: WorkspaceId -> S.ClientM (P.Producer (Persisted GsdEvent) IO ())
streamInfinitelyGsdEventsByWorkspaceIdOnPipe :: WorkspaceId -> S.ClientM (P.Producer (Persisted GsdEvent) IO ())
streamWorkspaceIdsCreatedOnPipe
  :<|> streamGsdCommandsByWorkspaceIdOnPipe
  :<|> streamInfinitelyGsdCommandsByWorkspaceIdOnPipe
  :<|> streamGsdEventsByWorkspaceIdOnPipe
  :<|> streamInfinitelyGsdEventsByWorkspaceIdOnPipe = S.client gsdMonitoringStreamingApi

streamWorkspaceIdsCreated :: IsStream stream => S.ClientM (stream IO (Persisted WorkspaceId) )
streamWorkspaceIdsCreated = fromPipes <$> streamWorkspaceIdsCreatedOnPipe

streamGsdCommandsByWorkspaceId :: IsStream stream => WorkspaceId -> S.ClientM (stream IO (Persisted GsdCommand) )
streamGsdCommandsByWorkspaceId workspaceId = fromPipes <$> (streamGsdCommandsByWorkspaceIdOnPipe workspaceId)


streamInfinitelyGsdCommandsByWorkspaceId :: IsStream stream => WorkspaceId -> S.ClientM (stream IO (Persisted GsdCommand) )
streamInfinitelyGsdCommandsByWorkspaceId workspaceId = fromPipes <$> (streamInfinitelyGsdCommandsByWorkspaceIdOnPipe workspaceId)

streamGsdEventsByWorkspaceId :: IsStream stream => WorkspaceId -> S.ClientM (stream IO (Persisted GsdEvent) )
streamGsdEventsByWorkspaceId workspaceId = fromPipes <$> (streamGsdEventsByWorkspaceIdOnPipe workspaceId)

streamInfinitelyGsdEventsByWorkspaceId :: IsStream stream => WorkspaceId -> S.ClientM (stream IO (Persisted GsdEvent) )
streamInfinitelyGsdEventsByWorkspaceId workspaceId = fromPipes <$> (streamInfinitelyGsdEventsByWorkspaceIdOnPipe workspaceId)