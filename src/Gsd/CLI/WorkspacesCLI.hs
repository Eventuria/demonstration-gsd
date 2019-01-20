{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Gsd.CLI.WorkspacesCLI (run)where

import Prelude hiding (length)
import System.Console.Byline hiding (askWithMenuRepeatedly)
import Gsd.CLI.ByLineWrapper (askWithMenuRepeatedly,renderPrefixAndSuffixForDynamicGsdMenu)
import qualified  Data.List as List
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
import Gsd.CLI.Greetings
import PersistedStreamEngine.Interface.PersistedItem
import Gsd.Read.Workspace

import qualified Gsd.CLI.WorkspaceCLI as WorkspaceActions (run)

data WorkspacesCommand = CreateWorkspaceRequest  Text
                       | ListWorkspaces Text
                       | GotoWorkOnAWorkspace Text
                       | Quit Text deriving Show


run :: WorkOnWorkspacesStepHandle
run clients @ Clients {writeApiUrl,gsdReadApiUrl} = do

  manager <- liftIO $ newManager defaultManagerSettings
  liftIO $ S.withClientM streamWorkspace (S.mkClientEnv manager gsdReadApiUrl) $ \e -> case e of
    Left err -> return $ ()
    Right streamWorkspace -> void $ runByline $ do
        workspaces <- liftIO $ streamWorkspace & Streamly.Prelude.toList
        let menuConfig = banner "> Available actions on the workspace set " $ menu (workspacesActions workspaces) stylizeAction
            prompt     = "> please choose an action (provide the index) : "
            onError    = "> please enter a valid index..."
            currentStep = WorkOnWorkspacesStep run clients

        answer <- askWithMenuRepeatedly menuConfig prompt onError
        case answer of
          CreateWorkspaceRequest description -> (runCreateWorkspaceRequest currentStep) >>= runNextStep
          ListWorkspaces description -> (runListWorkspaces currentStep) >>= runNextStep
          GotoWorkOnAWorkspace description -> (runWorkOnAWorkspace currentStep) >>= runNextStep
          Quit description -> runQuitCLI


  where
    workspacesActions :: [Persisted Workspace] -> [WorkspacesCommand]
    workspacesActions workspaces
      | List.length workspaces == 0 = [ CreateWorkspaceRequest     "Create A Workspace" ,
                                   Quit                       "Quit" ]
      | otherwise = [ CreateWorkspaceRequest     "Create A Workspace" ,
                      ListWorkspaces             "List Workspaces",
                      GotoWorkOnAWorkspace       "Work On A Workspace",
                      Quit                       "Quit" ]

    stylizeAction :: WorkspacesCommand -> Stylized
    stylizeAction workspacesAction = case workspacesAction of
      CreateWorkspaceRequest description ->  fg white <> text description
      ListWorkspaces description ->  fg white <> text description
      GotoWorkOnAWorkspace description ->  fg white <> text description
      Quit description ->  fg white <> text description


    runCreateWorkspaceRequest :: Step WorkOnWorkspaces -> Byline IO (Either StepError (Step WorkOnWorkspaces))
    runCreateWorkspaceRequest currentStep = do
      displayBeginningOfACommand
      workspaceId <- liftIO $ nextRandom
      commandId <- liftIO $ nextRandom
      sayLn $ fg cyan <> "generating a new Workspace Id (" <> text (toText workspaceId) <> ") "
      sayLn $ fg cyan <> "generating a new Command Id (" <> text (toText commandId) <>") "
      workspaceName <- askUntil ("> enter a workspace name : " ) Nothing atLeastThreeChars
      manager <- liftIO $ newManager defaultManagerSettings
      queryResult <- liftIO $ runClientM (sendCommand CreateWorkspace {commandId , workspaceId , workspaceName}) (mkClientEnv manager writeApiUrl)
      case queryResult of
        Left errorDescription -> return $ Left $ StepError {currentStep, errorDescription = show errorDescription }
        Right persistenceResult -> do
          sayLn $ fg cyan <> "Workpace "<> (text . toText) workspaceId <> " successfully created !"
          displayEndOfACommand
          return $ Right currentStep

    runWorkOnAWorkspace :: Step WorkOnWorkspaces -> Byline IO (Either StepError (Step WorkOnAWorkspace))
    runWorkOnAWorkspace currentStep @ (WorkOnWorkspacesStep workOnWorkspaces clients)  = do
      displayBeginningOfACommand
      manager <- liftIO $ newManager defaultManagerSettings
      result <- liftIO $ S.withClientM streamWorkspace (S.mkClientEnv manager gsdReadApiUrl) $ \e -> case e of
        Left err -> return $ Left $ show err
        Right streamWorkspace -> do
            workspaces <- streamWorkspace & Streamly.Prelude.toList
            return $ Right workspaces

      case result of
        Left  errorDescription -> return $ Left $ StepError {currentStep , errorDescription}
        Right workspaces -> do
            let menuConfig = banner (fg cyan <> "> available Workspaces ") $ renderPrefixAndSuffixForDynamicGsdMenu (menu workspaces stylizePersistedWorkspace)
                prompt     = "> please choose an action (provide the index) : "
                onError    = "> please enter a valid index..."
            (PersistedItem {item = workspace @ Workspace {workspaceId , workspaceName}}) <- askWithMenuRepeatedly menuConfig prompt onError
            displayEndOfACommand
            greetingOnWorkspace workspaceName
            return $ Right $ WorkOnAWorkspaceStep WorkspaceActions.run clients workspace workOnWorkspaces


    runListWorkspaces :: Step WorkOnWorkspaces -> Byline IO (Either StepError (Step WorkOnWorkspaces))
    runListWorkspaces currentStep = do
      displayBeginningOfACommand
      sayLn $ fg white <> "Workspaces"
      manager <- liftIO $ newManager defaultManagerSettings
      liftIO $ S.withClientM streamWorkspace (S.mkClientEnv manager gsdReadApiUrl) $ \e -> case e of
          Left errorDescription -> return $ Left $ StepError {currentStep,errorDescription = show errorDescription }
          Right stream -> do
            runStream $ stream
                & Streamly.Prelude.mapM (\persistedItem -> void $ runByline $ do sayLn $ fg white <> "  - " <> stylizePersistedWorkspace persistedItem )
            void $ runByline $ do displayEndOfACommand
            return $ Right currentStep

    stylizePersistedWorkspace :: Persisted Workspace -> Stylized
    stylizePersistedWorkspace PersistedItem {item = Workspace {workspaceName, workspaceId,
                                                               actionStats = ActionStats {total = totalActions,completed,opened},
                                                               goalStats = GoalStats {total = totalGoals,accomplished,toBeAccomplished }}} =
      fg cyan <> text  workspaceName <> fg white <>" -> Todo : " <> fg cyan <> (text . pack  .show) opened <> " goal(s) and " <> (text . pack  .show) toBeAccomplished <> " action(s)"


    atLeastThreeChars :: Text -> IO (Either Stylized Text)
    atLeastThreeChars input = return $
      if length input < 3
        then Left "3 characters minimum for a workspace please..."
        else Right input

