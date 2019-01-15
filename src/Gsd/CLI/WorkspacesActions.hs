{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Gsd.CLI.WorkspacesActions (run)where

import Prelude hiding (length)
import System.Console.Byline hiding (askWithMenuRepeatedly)
import Gsd.CLI.ByLineWrapper (askWithMenuRepeatedly)

import Data.Text
import Data.UUID.V4
import Data.UUID
import Control.Monad.IO.Class (MonadIO(liftIO))
import Gsd.Write.Client (sendCommand)
import Gsd.Write.Commands.Command
import Gsd.Read.Client (streamWorkspace)
import Gsd.Clients
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Gsd.CLI.Steps
import Servant.Client
import Data.Function ((&))
import Control.Monad (void)
import Gsd.CLI.QuitCLI (runQuitCLI)
import Streamly
import qualified Streamly.Prelude as Streamly.Prelude
import qualified Servant.Client.Streaming as S

import PersistedStreamEngine.Interface.PersistedItem
import Gsd.Read.Workspace
import qualified Gsd.CLI.WorkspaceActions as WorkspaceActions (run)

data WorkspacesAction = CreateWorkspaceRequest  Text
                       | ListWorkspaces Text
                       | GotoWorkOnAWorkspace Text
                       | Quit Text deriving Show


run :: WorkOnWorkspacesStepHandle
run clients @ Clients {writeApiUrl,gsdReadApiUrl} = do

  let menuConfig = banner "Available actions on the workspace set :" $ menu workspacesActions stylizeAction
      prompt     = "please choose an action (provide the index) : "
      onError    = "please enter a valid index..."
      currentStep = WorkOnWorkspacesStep run clients

  answer <- askWithMenuRepeatedly menuConfig prompt onError
  case answer of
    CreateWorkspaceRequest description -> (runCreateWorkspaceRequest currentStep) >>= runNextStep
    ListWorkspaces description -> (runListWorkspaces currentStep) >>= runNextStep
    GotoWorkOnAWorkspace description -> (runWorkOnAWorkspace currentStep) >>= runNextStep
    Quit description -> runQuitCLI


  where
    workspacesActions :: [WorkspacesAction]
    workspacesActions = [ CreateWorkspaceRequest "Create A Workspace" ,
                          ListWorkspaces         "List Workspaces",
                          GotoWorkOnAWorkspace       "Work On A Workspace",
                          Quit                   "Quit" ]

    stylizeAction :: WorkspacesAction -> Stylized
    stylizeAction workspacesAction = case workspacesAction of
      CreateWorkspaceRequest description ->  fg cyan <> text description
      ListWorkspaces description ->  fg cyan <> text description
      GotoWorkOnAWorkspace description ->  fg cyan <> text description
      Quit description ->  fg cyan <> text description


    runCreateWorkspaceRequest :: Step WorkOnWorkspaces -> Byline IO (Either StepError (Step WorkOnWorkspaces))
    runCreateWorkspaceRequest currentStep = do
      workspaceId <- liftIO $ nextRandom
      commandId <- liftIO $ nextRandom
      sayLn $ fg green <> "generating a new Workspace Id (" <> text (toText workspaceId) <> ") "
      sayLn $ fg green <> "generating a new Command Id (" <> text (toText commandId) <>") "
      workspaceName <- askUntil "Enter a workspace name : " Nothing atLeastThreeChars
      manager <- liftIO $ newManager defaultManagerSettings
      queryResult <- liftIO $ runClientM (sendCommand CreateWorkspace {commandId , workspaceId , workspaceName}) (mkClientEnv manager writeApiUrl)
      case queryResult of
        Left errorDescription -> return $ Left $ StepError {currentStep, errorDescription = show errorDescription }
        Right persistenceResult -> do
          sayLn $ fg green <> "Workpace "<> (text . toText) workspaceId <> " successfully created !"
          sayLn $ ""
          return $ Right currentStep

    runWorkOnAWorkspace :: Step WorkOnWorkspaces -> Byline IO (Either StepError (Step WorkOnAWorkspace))
    runWorkOnAWorkspace currentStep @ (WorkOnWorkspacesStep workOnWorkspaces clients)  = do
      manager <- liftIO $ newManager defaultManagerSettings
      result <- liftIO $ S.withClientM streamWorkspace (S.mkClientEnv manager gsdReadApiUrl) $ \e -> case e of
        Left err -> return $ Left $ show err
        Right streamWorkspace -> do
            workspaces <- streamWorkspace & Streamly.Prelude.toList
            return $ Right workspaces

      case result of
        Left  errorDescription -> return $ Left $ StepError {currentStep , errorDescription}
        Right workspaces -> do
            let menuConfig = banner "Available workspaces :" $ menu workspaces stylizePersistedWorkspace
                prompt     = "please choose an action (provide the index) : "
                onError    = "please enter a valid index..."
            (PersistedItem {item = workspace @ Workspace {workspaceId , workspaceName}}) <- askWithMenuRepeatedly menuConfig prompt onError
            sayLn $ fg green <> (text . pack . show) workspace <> " selected !"
            sayLn $ ""
            return $ Right $ WorkOnAWorkspaceStep WorkspaceActions.run clients workspace workOnWorkspaces


    runListWorkspaces :: Step WorkOnWorkspaces -> Byline IO (Either StepError (Step WorkOnWorkspaces))
    runListWorkspaces currentStep = do
      sayLn $ fg green <> "Listing all workspaces created : "
      manager <- liftIO $ newManager defaultManagerSettings
      liftIO $ S.withClientM streamWorkspace (S.mkClientEnv manager gsdReadApiUrl) $ \e -> case e of
          Left errorDescription -> return $ Left $ StepError {currentStep,errorDescription = show errorDescription }
          Right stream -> do
            runStream $ stream
                & Streamly.Prelude.mapM (\PersistedItem { offset = offset, item = workspace} -> void $ runByline $ do
                  sayLn $ fg green <> (text . pack . show) workspace)
            return $ Right currentStep

    stylizePersistedWorkspace :: Persisted Workspace -> Stylized
    stylizePersistedWorkspace PersistedItem {item = Workspace {workspaceId,workspaceName}} =
      fg cyan <> "Workspace (" <> (text $ toText workspaceId) <> " , " <> text workspaceName <> " )"


    atLeastThreeChars :: Text -> IO (Either Stylized Text)
    atLeastThreeChars input = return $
      if length input < 3
        then Left "3 characters minimum for a workspace please..."
        else Right input

