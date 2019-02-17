{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Gsd.Read.API.Client.Client (
  fetchWorkspaces,
  fetchWorkspace,
  fetchGoals,
  fetchGoal,
  fetchActions) where

import Data.Proxy
import Servant
import Gsd.Read.Model.Workspace
import qualified Pipes as P
import Streamly.Adapters
import qualified Servant.Client.Streaming as S
import Gsd.Write.Model.Core
import Gsd.Read.Model.Goal
import Gsd.Read.Model.Action
import PersistedStreamEngine.Interface.PersistedItem
import System.SafeResponse
import Servant.Pipes ()
import Gsd.Read.API.Client.State
import qualified Streamly.Safe as StreamlySafe
import Logger.Core
import Control.Exception
import Gsd.Read.API.Definition

fetchWorkspaces :: State ->
                   IO (SafeResponse [Persisted Workspace])
fetchWorkspaces clientSetting =
  bindWithSettings
    clientSetting
    streamWorkspaceOnPipe

fetchGoals :: State ->
              WorkspaceId ->
              IO (SafeResponse [Goal])
fetchGoals clientSetting workspaceId =
  bindWithSettings
    clientSetting
    (streamGoalOnPipe workspaceId)

fetchActions :: State ->
                WorkspaceId ->
                GoalId ->
                IO (SafeResponse [Action])
fetchActions clientSetting workspaceId goalId =
  bindWithSettings
    clientSetting
    (streamActionOnPipe workspaceId goalId)

bindWithSettings :: State ->
                    S.ClientM (P.Producer (SafeResponse (item)) IO ()) ->
                    IO (SafeResponse [item])
bindWithSettings State { httpClientManager, url, logger} call = do
  (S.withClientM
     (fromPipes <$> call )
     (S.mkClientEnv httpClientManager url)
     (\e -> case e of
        Left errorHttpLevel -> do
         logInfo logger "An http error occured with the monitoring microservice."
         return $ Left $ toException errorHttpLevel
        Right stream -> do
         safeResponse <- StreamlySafe.toList stream
         return safeResponse))

fetchGoal :: State ->
              WorkspaceId ->
              GoalId ->
              IO (SafeResponse (Maybe Goal))
fetchGoal State { httpClientManager, url, logger} workspaceId goalId =
  (S.withClientM
       (fetchGoalCall workspaceId goalId)
       (S.mkClientEnv httpClientManager url)
       (\e -> case e of
          Left errorHttpLevel -> do
           logInfo logger "An http error occured with the monitoring microservice."
           return $ Left $ toException errorHttpLevel
          Right safeResponse -> return safeResponse))

fetchWorkspace :: State ->
              WorkspaceId ->
              IO (SafeResponse (Maybe Workspace))
fetchWorkspace State { httpClientManager, url, logger} workspaceId  =
  (S.withClientM
       (fetchWorkspaceCall workspaceId)
       (S.mkClientEnv httpClientManager url)
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

