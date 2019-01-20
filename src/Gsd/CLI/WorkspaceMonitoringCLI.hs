{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
module Gsd.CLI.WorkspaceMonitoringCLI
(runListCommandReceived,
 runListCommandResponseReceived,
 runListEventsGenerated,
 runListValidationStateHistory) where

import System.Console.Byline
import qualified Servant.Client.Streaming as S
import qualified Streamly.Prelude as Streamly.Prelude
import Control.Monad.IO.Class (MonadIO(liftIO))
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Control.Monad (void)
import Data.Text
import Streamly
import Data.Function ((&))
import Gsd.CLI.Steps
import Gsd.Read.Workspace

import Gsd.Monitoring.Client (streamGsdEventByWorkspaceId,
                              streamGsdCommandByWorkspaceId,
                              streamGsdCommandResponseByWorkspaceId,
                              streamGsdValidationStateByWorkspaceId)

runListCommandReceived :: forall stepType. Step stepType -> S.BaseUrl -> Workspace -> Byline IO (Either StepError (Step stepType))
runListCommandReceived currentStep gsdMonitoringApiUrl Workspace {workspaceId} = do
  manager <- liftIO $ newManager defaultManagerSettings
  liftIO $ S.withClientM (streamGsdCommandByWorkspaceId workspaceId) (S.mkClientEnv manager gsdMonitoringApiUrl) $ \e -> case e of
     Left errorDescription -> return $ Left $ StepError {currentStep, errorDescription = show errorDescription }
     Right streamGsdCommandByWorkspaceId -> do
       runStream $ streamGsdCommandByWorkspaceId
             & Streamly.Prelude.mapM (\persistedGsdCommand -> void $ runByline $ do
               sayLn $ fg green <> (text . pack . show) persistedGsdCommand)
       return $ Right currentStep

runListCommandResponseReceived :: forall stepType. Step stepType -> S.BaseUrl -> Workspace -> Byline IO (Either StepError (Step stepType))
runListCommandResponseReceived currentStep gsdMonitoringApiUrl Workspace {workspaceId} = do
  manager <- liftIO $ newManager defaultManagerSettings
  liftIO $ S.withClientM (streamGsdCommandResponseByWorkspaceId workspaceId) (S.mkClientEnv manager gsdMonitoringApiUrl) $ \e -> case e of
     Left errorDescription -> return $ Left $ StepError {currentStep, errorDescription = show errorDescription }
     Right streamGsdCommandResponseByWorkspaceId -> do
       runStream $ streamGsdCommandResponseByWorkspaceId
             & Streamly.Prelude.mapM (\persistedGsdCommandResponse -> void $ runByline $ do
               sayLn $ fg green <> (text . pack . show) persistedGsdCommandResponse)
       return $ Right currentStep

runListEventsGenerated :: forall stepType. Step stepType -> S.BaseUrl -> Workspace -> Byline IO (Either StepError (Step stepType))
runListEventsGenerated currentStep gsdMonitoringApiUrl Workspace {workspaceId} = do
  manager <- liftIO $ newManager defaultManagerSettings
  liftIO $ S.withClientM (streamGsdEventByWorkspaceId workspaceId) (S.mkClientEnv manager gsdMonitoringApiUrl) $ \e -> case e of
     Left errorDescription -> return $ Left $ StepError {currentStep, errorDescription = show errorDescription }
     Right streamGsdEventsByWorkspaceId -> do
       runStream $ streamGsdEventsByWorkspaceId
             & Streamly.Prelude.mapM (\persistedGsdEvent -> void $ runByline $ do
               sayLn $ fg green <> (text . pack . show) persistedGsdEvent)
       return $ Right currentStep

runListValidationStateHistory :: forall stepType. Step stepType -> S.BaseUrl -> Workspace -> Byline IO (Either StepError (Step stepType))
runListValidationStateHistory currentStep gsdMonitoringApiUrl Workspace {workspaceId} = do
  manager <- liftIO $ newManager defaultManagerSettings
  liftIO $ S.withClientM (streamGsdValidationStateByWorkspaceId workspaceId) (S.mkClientEnv manager gsdMonitoringApiUrl) $ \e -> case e of
     Left errorDescription -> return $ Left $ StepError {currentStep, errorDescription = show errorDescription }
     Right streamGsdEventsByWorkspaceId -> do
       runStream $ streamGsdEventsByWorkspaceId
             & Streamly.Prelude.mapM (\persistedValidationState -> void $ runByline $ do
               sayLn $ fg green <> (text . pack . show) persistedValidationState)
       return $ Right currentStep
