{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Gsd.CLI.UI.Workspaces (run)where

import Prelude hiding (length)
import System.Console.Byline hiding (askWithMenuRepeatedly)
import ByLine.Wrapper (askWithMenuRepeatedly,renderPrefixAndSuffixForDynamicGsdMenu)
import qualified  Data.List as List
import Data.Text hiding (map,foldr)
import Data.UUID.V4
import Data.UUID
import Control.Monad.IO.Class (MonadIO(liftIO))
import Gsd.Write.Flow.Sourcer.Client.Client
import Gsd.Write.Model.Commands.Command
import Gsd.Read.API.Client.Client (fetchWorkspaces )
import Gsd.CLI.Dependencies
import Gsd.CLI.Workflow.Steps
import Gsd.CLI.UI.Quit (runQuitCLI)
import Gsd.CLI.UI.Greetings
import PersistedStreamEngine.Interface.PersistedItem
import Gsd.Read.Model.Workspace
import CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import qualified Gsd.CLI.UI.Workspace as WorkspaceActions (run)
import Gsd.Read.Model.GoalStats


data WorkspacesCommand = -- Workspaces Command
                         CreateWorkspaceCommand  Text
                         -- Navigation
                       | GotoWorkOnAWorkspace    Text
                       | QuitCommand             Text


run :: WorkOnWorkspacesStepHandle
run cliDependencies @ Dependencies {readClientDependencies , writeClientDependencies} = do
  let currentStep = WorkOnWorkspacesStep run cliDependencies
  safeResponse <- liftIO $ fetchWorkspaces readClientDependencies
  case safeResponse of
    Left error -> runNextStep $ Left StepError {currentStep, errorDescription = show error }
    Right persistedWorkspaces -> do
      let workspaces =  (\PersistedItem{item} -> item) <$> persistedWorkspaces
      displayWorkspacesState workspaces
      sayLn "Commands"
      let menuConfig = renderPrefixAndSuffixForDynamicGsdMenu $
                        menu (workspacesActions workspaces) stylizeAction
          prompt     = "> please choose an action (provide the index) : "
          onError    = "> please enter a valid index..."
          currentStep = WorkOnWorkspacesStep run cliDependencies

      answer <- askWithMenuRepeatedly menuConfig prompt onError
      case answer of
        CreateWorkspaceCommand _ -> runCreateWorkspaceRequest currentStep >>= runNextStep
        GotoWorkOnAWorkspace _ ->   runWorkOnAWorkspace       currentStep >>= runNextStep
        QuitCommand _ ->            runQuitCLI                            >>= runNextStep


  where
    workspacesActions :: [Workspace] -> [WorkspacesCommand]
    workspacesActions workspaces
      | List.length workspaces == 0 = [ CreateWorkspaceCommand     "Create A Workspace" ,
                                        QuitCommand                       "Quit" ]
      | otherwise = [ CreateWorkspaceCommand     "Create A Workspace" ,
                      GotoWorkOnAWorkspace       "Work On A Workspace",
                      QuitCommand                       "Quit" ]

    stylizeAction :: WorkspacesCommand -> Stylized
    stylizeAction workspacesAction = case workspacesAction of
      CreateWorkspaceCommand description ->  fg white <> text description
      GotoWorkOnAWorkspace description ->  fg white <> text description
      QuitCommand description ->  fg white <> text description


    runCreateWorkspaceRequest :: Step WorkOnWorkspaces -> Byline IO (Either StepError (Step WorkOnWorkspaces))
    runCreateWorkspaceRequest currentStep = do
      displayBeginningOfACommand
      workspaceId <- liftIO $ nextRandom
      commandId <- liftIO $ nextRandom
      sayLn $ fg cyan <> "generating a new Workspace Id (" <> text (toText workspaceId) <> ") "
      sayLn $ fg cyan <> "generating a new Command Id (" <> text (toText commandId) <>") "
      workspaceName <- askUntil ("> enter a workspace name : " ) Nothing atLeastThreeChars

      response <- liftIO $ sendCommandAndWaitTillProcessed writeClientDependencies  CreateWorkspace {
                                                                    commandId ,
                                                                    workspaceId ,
                                                                    workspaceName}
      case response of
        Left  errorDescription -> return $ Left $ StepError {currentStep , errorDescription = show errorDescription}
        Right CommandFailed {reason} ->  do
          sayLn $ fg red <> "> The command failed : "<> (text . pack ) reason
          displayEndOfACommand
          return $ Right currentStep
        Right CommandSuccessfullyProcessed {} ->  do
          sayLn $ fg green <> "> The command has been successfully processed... "
          displayEndOfACommand
          return $ Right currentStep


    runWorkOnAWorkspace :: Step WorkOnWorkspaces -> Byline IO (Either StepError (Step WorkOnAWorkspace))
    runWorkOnAWorkspace currentStep @ (WorkOnWorkspacesStep
                                            workOnWorkspaces
                                            cliDependencies @ Dependencies {readClientDependencies,writeClientDependencies})  = do
      displayBeginningOfACommand
      safeResponse <- liftIO $ fetchWorkspaces readClientDependencies
      case safeResponse of
        Left stepError -> return $ Left StepError {currentStep, errorDescription = show stepError }
        Right persistedWorkspaces -> do
          let workspaces =  (\PersistedItem{item} -> item) <$> persistedWorkspaces
          sayLn "Workspaces"
          let menuConfig = renderPrefixAndSuffixForDynamicGsdMenu (menu workspaces displayWorkspaceState)
              prompt     = "> please choose an action (provide the index) : "
              onError    = "> please enter a valid index..."
          workspace <- askWithMenuRepeatedly
                          menuConfig
                          prompt
                          onError
          displayEndOfACommand
          return $ Right $ WorkOnAWorkspaceStep WorkspaceActions.run cliDependencies workspace workOnWorkspaces


    displayWorkspacesState :: [Workspace] -> Byline IO ()
    displayWorkspacesState workspaces
      | (List.length workspaces) == 0 = return ()
      | otherwise =
        sayLn $
             fg white <> "Workspaces\n"
          <> (foldr
               (<>)
                ""
                (map (\workspace -> fg white <> "  - "<> displayWorkspaceState workspace <> "\n" )
                workspaces))
          <> fg white <> "------------------------------------------"

    displayWorkspaceState :: Workspace -> Stylized
    displayWorkspaceState  Workspace {workspaceName,
                                      workspaceId,
                                      goalStats = GoalStats {total = totalGoals,
                                                            accomplished,
                                                            toBeAccomplished }} =
      fg green <> text  workspaceName
              <> fg white <>" > Todo : "
              <> fg green <> (text . pack  .show) toBeAccomplished <> " goal(s)"


    atLeastThreeChars :: Text -> IO (Either Stylized Text)
    atLeastThreeChars input = return $
      if length input < 3
        then Left "3 characters minimum for a workspace please..."
        else Right input

