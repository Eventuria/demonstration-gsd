{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Gsd.Read.Client (streamWorkspace,streamGoal) where

import Data.Proxy
import Servant
import Gsd.Read.Workspace
import qualified Pipes as P
import Streamly.Adapters
import Gsd.Read.WebStreamingApi
import qualified Servant.Client.Streaming as S
import Gsd.Write.Core
import Gsd.Read.Goal
import PersistedStreamEngine.Interface.PersistedItem

import Servant.Pipes ()

import Streamly

gsdReadStreamingApi :: Proxy GSDReadStreamingApi
gsdReadStreamingApi = Proxy

streamWorkspaceOnPipe :: S.ClientM (P.Producer (Persisted Workspace) IO () )
streamGoalOnPipe :: WorkspaceId -> S.ClientM (P.Producer Goal IO () )
streamWorkspaceOnPipe :<|> streamGoalOnPipe  = S.client gsdReadStreamingApi

streamWorkspace :: IsStream stream => S.ClientM (stream IO (Persisted Workspace) )
streamWorkspace = fromPipes <$> streamWorkspaceOnPipe

streamGoal :: IsStream stream => WorkspaceId -> S.ClientM (stream IO (Goal) )
streamGoal workspaceId = fromPipes <$> (streamGoalOnPipe workspaceId)