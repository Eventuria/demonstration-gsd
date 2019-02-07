{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Gsd.Read.Client (streamWorkspace,streamGoal,streamAction,fetchWorkspace,fetchGoal) where

import Data.Proxy
import Servant
import Gsd.Read.Workspace
import qualified Pipes as P
import Streamly.Adapters
import Gsd.Read.WebApi
import qualified Servant.Client.Streaming as S
import Gsd.Write.Core
import Gsd.Read.Goal
import Gsd.Read.Action
import PersistedStreamEngine.Interface.PersistedItem
import System.SafeResponse
import Servant.Pipes ()

import Streamly

gsdReadApi :: Proxy GSDReadApi
gsdReadApi = Proxy

streamWorkspaceOnPipe :: S.ClientM (P.Producer (SafeResponse (Persisted Workspace)) IO () )
fetchWorkspace :: WorkspaceId -> S.ClientM (SafeResponse (Maybe Workspace))
fetchGoal :: WorkspaceId -> GoalId ->  S.ClientM (SafeResponse (Maybe Goal))
streamGoalOnPipe :: WorkspaceId -> S.ClientM (P.Producer (SafeResponse Goal )IO () )
streamActionOnPipe :: WorkspaceId -> GoalId -> S.ClientM (P.Producer (SafeResponse Action) IO () )
streamWorkspaceOnPipe :<|> streamGoalOnPipe :<|> streamActionOnPipe :<|> fetchWorkspace :<|> fetchGoal = S.client gsdReadApi


streamWorkspace :: IsStream stream => S.ClientM (stream IO (SafeResponse (Persisted Workspace)) )
streamWorkspace = fromPipes <$> streamWorkspaceOnPipe

streamGoal :: IsStream stream => WorkspaceId -> S.ClientM (stream IO (SafeResponse (Goal)) )
streamGoal workspaceId = fromPipes <$> (streamGoalOnPipe workspaceId)

streamAction :: IsStream stream => WorkspaceId -> GoalId -> S.ClientM (stream IO (SafeResponse (Action)) )
streamAction workspaceId goalId = fromPipes <$> (streamActionOnPipe workspaceId goalId)