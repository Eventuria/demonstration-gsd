{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Gsd.Read.Client (
  fetchWorkspaces,
  fetchWorkspace,
  fetchGoals,
  fetchGoal,
  fetchActions) where

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
import Gsd.Clients
import qualified Streamly.Safe as StreamlySafe
import Logger.Core
import Control.Exception

fetchWorkspaces :: ClientSetting ->
                   IO (SafeResponse [Persisted Workspace])
fetchWorkspaces clientSetting =
  bindWithSettings
    clientSetting
    streamWorkspaceOnPipe

fetchGoals :: ClientSetting ->
              WorkspaceId ->
              IO (SafeResponse [Goal])
fetchGoals clientSetting workspaceId =
  bindWithSettings
    clientSetting
    (streamGoalOnPipe workspaceId)

fetchActions :: ClientSetting ->
                WorkspaceId ->
                GoalId ->
                IO (SafeResponse [Action])
fetchActions clientSetting workspaceId goalId =
  bindWithSettings
    clientSetting
    (streamActionOnPipe workspaceId goalId)

bindWithSettings :: ClientSetting ->
                    S.ClientM (P.Producer (SafeResponse (item)) IO ()) ->
                    IO (SafeResponse [item])
bindWithSettings ClientSetting { manager, url, logger} call = do
  (S.withClientM
     (fromPipes <$> call )
     (S.mkClientEnv manager url)
     (\e -> case e of
        Left errorHttpLevel -> do
         logInfo logger "An http error occured with the monitoring microservice."
         return $ Left $ toException errorHttpLevel
        Right stream -> do
         safeResponse <- StreamlySafe.toList stream
         return safeResponse))

fetchGoal :: ClientSetting ->
              WorkspaceId ->
              GoalId ->
              IO (SafeResponse (Maybe Goal))
fetchGoal ClientSetting { manager, url, logger} workspaceId goalId =
  (S.withClientM
       (fetchGoalCall workspaceId goalId)
       (S.mkClientEnv manager url)
       (\e -> case e of
          Left errorHttpLevel -> do
           logInfo logger "An http error occured with the monitoring microservice."
           return $ Left $ toException errorHttpLevel
          Right safeResponse -> return safeResponse))

fetchWorkspace :: ClientSetting ->
              WorkspaceId ->
              IO (SafeResponse (Maybe Workspace))
fetchWorkspace ClientSetting { manager, url, logger} workspaceId  =
  (S.withClientM
       (fetchWorkspaceCall workspaceId)
       (S.mkClientEnv manager url)
       (\e -> case e of
          Left errorHttpLevel -> do
           logInfo logger "An http error occured with the monitoring microservice."
           return $ Left $ toException errorHttpLevel
          Right safeResponse -> return safeResponse))

fetchWorkspaceCall :: WorkspaceId ->            S.ClientM (SafeResponse (Maybe Workspace))
fetchGoalCall ::      WorkspaceId -> GoalId ->  S.ClientM (SafeResponse (Maybe Goal))
streamWorkspaceOnPipe ::                        S.ClientM (P.Producer (SafeResponse (Persisted Workspace)) IO () )
streamGoalOnPipe ::   WorkspaceId ->            S.ClientM (P.Producer (SafeResponse Goal )IO () )
streamActionOnPipe :: WorkspaceId -> GoalId ->  S.ClientM (P.Producer (SafeResponse Action) IO () )
streamWorkspaceOnPipe
  :<|> streamGoalOnPipe
  :<|> streamActionOnPipe
  :<|> fetchWorkspaceCall
  :<|> fetchGoalCall = S.client gsdReadApi
 where
  gsdReadApi :: Proxy GSDReadApi
  gsdReadApi = Proxy

