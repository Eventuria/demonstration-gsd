{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Eventuria.GSD.Read.API.Client.Client (
  healthCheck,
  fetchWorkspaces,
  fetchWorkspace,
  fetchGoals,
  fetchGoal,
  fetchActions) where

import Data.Proxy
import Servant
import Eventuria.GSD.Read.Model.Workspace
import qualified Pipes as P
import Eventuria.Adapters.Streamly.Adapters
import qualified Servant.Client.Streaming as S
import Eventuria.GSD.Write.Model.Core
import Eventuria.GSD.Read.Model.Goal
import Eventuria.GSD.Read.Model.Action
import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import Eventuria.Commons.System.SafeResponse
import Servant.Pipes ()
import Eventuria.GSD.Read.API.Client.State
import qualified Eventuria.Adapters.Streamly.Safe as StreamlySafe
import Eventuria.Commons.Logger.Core
import Control.Exception
import Eventuria.GSD.Read.API.Definition
import Eventuria.Commons.DevOps.Core

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
fetchWorkspaceCall :: WorkspaceId ->            S.ClientM (SafeResponse (Maybe Workspace))
fetchGoalCall ::      WorkspaceId -> GoalId ->  S.ClientM (SafeResponse (Maybe Goal))
streamWorkspaceOnPipe ::                        S.ClientM (P.Producer (SafeResponse (Persisted Workspace)) IO () )
streamGoalOnPipe ::   WorkspaceId ->            S.ClientM (P.Producer (SafeResponse Goal )IO () )
streamActionOnPipe :: WorkspaceId -> GoalId ->  S.ClientM (P.Producer (SafeResponse Action) IO () )
healthCheckCall
  :<|> streamWorkspaceOnPipe
  :<|> streamGoalOnPipe
  :<|> streamActionOnPipe
  :<|> fetchWorkspaceCall
  :<|> fetchGoalCall = S.client gsdReadApi
 where
  gsdReadApi :: Proxy GSDReadApi
  gsdReadApi = Proxy

