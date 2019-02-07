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
import Control.Monad.IO.Class (MonadIO(liftIO))
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Control.Monad (void)
import Data.Text
import Data.Function ((&))
import Gsd.CLI.Steps
import Gsd.Read.Workspace
import PersistedStreamEngine.Interface.PersistedItem
import Gsd.CLI.Greetings
import qualified Streamly.Safe as StreamlySafe
import Gsd.Monitoring.Client (streamGsdEventByWorkspaceId,
                              streamGsdCommandByWorkspaceId,
                              streamGsdCommandResponseByWorkspaceId,
                              streamGsdValidationStateByWorkspaceId)

runListCommandReceived :: forall stepType. Step stepType ->
                                           S.BaseUrl ->
                                           Workspace ->
                                           Byline IO (Either StepError (Step stepType))
runListCommandReceived currentStep gsdMonitoringApiUrl Workspace {workspaceId} = do
  displayBeginningOfACommand
  sayLn $ fg white <> "Commands"
  manager <- liftIO $ newManager defaultManagerSettings
  result <- liftIO $ S.withClientM
                        (streamGsdCommandByWorkspaceId workspaceId)
                        (S.mkClientEnv manager gsdMonitoringApiUrl)
                        $ \e -> case e of
                           Left errorHttpLevel -> return $ Left $ StepError {
                                                                      currentStep,
                                                                      errorDescription = show errorHttpLevel }
                           Right streamGsdEventsByWorkspaceId -> do
                             response <- StreamlySafe.toList $ streamGsdEventsByWorkspaceId
                             case (response) of
                                Left errorApplicationLevel -> return $ Left $ StepError {
                                                                            currentStep,
                                                                            errorDescription = show errorApplicationLevel }
                                Right persistedGsdCommands -> do
                                     persistedGsdCommands
                                      & mapM(\persistedGsdCommand ->
                                          void $ runByline $ sayLn $ displayItem persistedGsdCommand)
                                     return $ Right currentStep
  displayEndOfACommand
  return result

runListCommandResponseReceived :: forall stepType. Step stepType ->
                                                   S.BaseUrl ->
                                                   Workspace ->
                                                   Byline IO (Either StepError (Step stepType))
runListCommandResponseReceived currentStep gsdMonitoringApiUrl Workspace {workspaceId} = do
  displayBeginningOfACommand
  sayLn $ fg white <> "Command Responses"
  manager <- liftIO $ newManager defaultManagerSettings
  result <- liftIO $ S.withClientM
                        (streamGsdCommandResponseByWorkspaceId workspaceId)
                        (S.mkClientEnv manager gsdMonitoringApiUrl)
                        $ \e -> case e of
                           Left errorHttpLevel -> return $ Left $ StepError {
                                                                      currentStep,
                                                                      errorDescription = show errorHttpLevel }
                           Right streamGsdEventsByWorkspaceId -> do
                             response <- StreamlySafe.toList $ streamGsdEventsByWorkspaceId
                             case (response) of
                                Left errorApplicationLevel -> return $ Left $ StepError {
                                                                            currentStep,
                                                                            errorDescription = show errorApplicationLevel }
                                Right persistedGsdCommandResponses -> do
                                     persistedGsdCommandResponses
                                      & mapM(\persistedGsdCommandResponse ->
                                          void $ runByline $ sayLn $ displayItem persistedGsdCommandResponse)
                                     return $ Right currentStep
  displayEndOfACommand
  return result

runListEventsGenerated :: forall stepType. Step stepType ->
                                           S.BaseUrl ->
                                           Workspace ->
                                           Byline IO (Either StepError (Step stepType))
runListEventsGenerated currentStep gsdMonitoringApiUrl Workspace {workspaceId} = do
  displayBeginningOfACommand
  sayLn $ fg white <> "Events"
  manager <- liftIO $ newManager defaultManagerSettings
  result <- liftIO $ S.withClientM
                        (streamGsdEventByWorkspaceId workspaceId)
                        (S.mkClientEnv manager gsdMonitoringApiUrl)
                        $ \e -> case e of
                           Left errorHttpLevel -> return $ Left $ StepError {
                                                                      currentStep,
                                                                      errorDescription = show errorHttpLevel }
                           Right streamGsdEventsByWorkspaceId -> do
                             response <- StreamlySafe.toList $ streamGsdEventsByWorkspaceId
                             case (response) of
                                Left errorApplicationLevel -> return $ Left $ StepError {
                                                                            currentStep,
                                                                            errorDescription = show errorApplicationLevel }
                                Right persistedGsdEvents -> do
                                     persistedGsdEvents
                                      & mapM(\persistedGsdEvent ->
                                          void $ runByline $ sayLn $ displayItem persistedGsdEvent)
                                     return $ Right currentStep
  displayEndOfACommand
  return result

runListValidationStateHistory :: forall stepType. Step stepType ->
                                                  S.BaseUrl ->
                                                  Workspace ->
                                                  Byline IO (Either StepError (Step stepType))
runListValidationStateHistory currentStep gsdMonitoringApiUrl Workspace {workspaceId} = do
  displayBeginningOfACommand
  sayLn $ fg white <> "Validation State History"
  manager <- liftIO $ newManager defaultManagerSettings
  result <- liftIO $ S.withClientM
                        (streamGsdValidationStateByWorkspaceId workspaceId)
                        (S.mkClientEnv manager gsdMonitoringApiUrl)
                        $ \e -> case e of
                           Left errorHttpLevel -> return $ Left $ StepError {
                                                                      currentStep,
                                                                      errorDescription = show errorHttpLevel }
                           Right streamGsdEventsByWorkspaceId -> do
                             response <- StreamlySafe.toList $ streamGsdEventsByWorkspaceId
                             case (response) of
                                Left errorApplicationLevel -> return $ Left $ StepError {
                                                                            currentStep,
                                                                            errorDescription = show errorApplicationLevel }
                                Right persistedValidationStates -> do
                                     persistedValidationStates
                                      & mapM(\persistedValidationState ->
                                          void $ runByline $ sayLn $ displayItem persistedValidationState)
                                     return $ Right currentStep
  displayEndOfACommand
  return result

displayItem :: Show item => (Persisted item) -> Stylized
displayItem PersistedItem {offset, item} =
  fg white <> "  "<>(text.pack.show) offset <> "- " <> fg green <> (text.pack.show) item

