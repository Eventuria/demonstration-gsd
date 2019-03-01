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
  fetchActions,
  ReadServerDown(..)) where

import           Control.Exception
                 
import           Data.Proxy
                 
import           Servant
import           Servant.Pipes ()

import qualified Servant.Client.Streaming as S
import qualified Pipes as P
import qualified Streamly.Prelude as Streamly

import           Eventuria.Adapters.Streamly.Adapters

import           Eventuria.Commons.DevOps.Core

import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem

import           Eventuria.GSD.Read.Model.Workspace
import           Eventuria.GSD.Write.Model.Core
import           Eventuria.GSD.Read.Model.Goal
import           Eventuria.GSD.Read.Model.Action
import           Eventuria.GSD.Read.API.Client.Dependencies
import           Eventuria.GSD.Read.API.Definition

data ReadServerDown = ReadServerDown  deriving Show

instance Exception ReadServerDown

fetchWorkspaces :: Dependencies ->
                   IO (Either ReadServerDown [Persisted Workspace])
fetchWorkspaces dependencies =
  bindWithSettings
    dependencies
    streamWorkspaceOnPipe

fetchGoals :: Dependencies ->
              WorkspaceId ->
              IO (Either ReadServerDown [Goal])
fetchGoals dependencies workspaceId =
  bindWithSettings
    dependencies
    (streamGoalOnPipe workspaceId)

fetchActions :: Dependencies ->
                WorkspaceId ->
                GoalId ->
                IO (Either ReadServerDown [Action])
fetchActions dependencies workspaceId goalId =
  bindWithSettings
    dependencies
    (streamActionOnPipe workspaceId goalId)

bindWithSettings :: Dependencies ->
                    S.ClientM (P.Producer (item) IO ()) ->
                    IO (Either ReadServerDown [item])
bindWithSettings Dependencies { httpClientManager, url, logger} call =
  catch
    (S.withClientM
       (fromPipes <$> call )
       (S.mkClientEnv httpClientManager url)
       (\e -> case e of
          Left errorHttpLevel -> return $ Left $ ReadServerDown
          Right stream -> do
           list <- Streamly.toList stream
           return $ Right list))
    (\SomeException {} -> return $ Left $ ReadServerDown )


fetchGoal :: Dependencies ->
              WorkspaceId ->
              GoalId ->
              IO (Either ReadServerDown (Maybe Goal))
fetchGoal Dependencies { httpClientManager, url, logger} workspaceId goalId =
  catch
    (S.withClientM
         (fetchGoalCall workspaceId goalId)
         (S.mkClientEnv httpClientManager url)
         (\e -> case e of
            Left errorHttpLevel -> return $ Left ReadServerDown
            Right maybeGoal -> return $ Right maybeGoal))
    (\SomeException {} -> return $ Left $ ReadServerDown )

fetchWorkspace :: Dependencies ->
              WorkspaceId ->
              IO (Either ReadServerDown (Maybe Workspace))
fetchWorkspace Dependencies { httpClientManager, url, logger} workspaceId  =
  catch
    (S.withClientM
         (fetchWorkspaceCall workspaceId)
         (S.mkClientEnv httpClientManager url)
         (\e -> case e of
            Left errorHttpLevel -> return $ Left  ReadServerDown
            Right maybeWorkspace -> return $ Right maybeWorkspace))
    (\SomeException {} -> return $ Left $ ReadServerDown )

healthCheck :: Dependencies -> IO (Either ReadServerDown Healthy)
healthCheck    Dependencies { httpClientManager, url, logger}  =
  catch
    (S.withClientM
       healthCheckCall
       (S.mkClientEnv httpClientManager url)
       (\e -> do
          case e of
            Left errorHttpLevel -> return $ Left ReadServerDown
            Right healthy  -> return $ Right () ))
    (\SomeException {} -> return $ Left ReadServerDown )

healthCheckCall :: S.ClientM Healthy
fetchWorkspaceCall :: WorkspaceId ->            S.ClientM (Maybe Workspace)
fetchGoalCall ::      WorkspaceId -> GoalId ->  S.ClientM (Maybe Goal)
streamWorkspaceOnPipe ::                        S.ClientM (P.Producer (Persisted Workspace) IO () )
streamGoalOnPipe ::   WorkspaceId ->            S.ClientM (P.Producer Goal IO () )
streamActionOnPipe :: WorkspaceId -> GoalId ->  S.ClientM (P.Producer Action IO () )
healthCheckCall
  :<|> streamWorkspaceOnPipe
  :<|> streamGoalOnPipe
  :<|> streamActionOnPipe
  :<|> fetchWorkspaceCall
  :<|> fetchGoalCall = S.client gsdReadApi
 where
  gsdReadApi :: Proxy GSDReadApi
  gsdReadApi = Proxy

