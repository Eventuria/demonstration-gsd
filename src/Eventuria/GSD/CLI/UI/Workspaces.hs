{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.GSD.CLI.UI.Workspaces (run)where

import           Prelude hiding (length,read)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Text hiding (map,foldr)
import           Data.UUID.V4
import qualified Data.List                      as List

import           System.Console.Byline hiding (askWithMenuRepeatedly)

import           Eventuria.Adapters.ByLine.Wrapper (askWithMenuRepeatedly,renderPrefixAndSuffixForDynamicGsdMenu)
import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse

import           Eventuria.GSD.Write.CommandSourcer.Client.Client
import           Eventuria.GSD.Write.Model.Commands.Command
import           Eventuria.GSD.Read.API.Client.Client (fetchWorkspaces )
import           Eventuria.GSD.CLI.Dependencies
import           Eventuria.GSD.CLI.Workflow.Steps
import           Eventuria.GSD.CLI.UI.Quit (runQuitCLI)
import           Eventuria.GSD.CLI.UI.Greetings
import           Eventuria.GSD.Read.Model.Workspace
import qualified Eventuria.GSD.CLI.UI.Workspace as WorkspaceActions (run)
import           Eventuria.GSD.Read.Model.GoalStats


data WorkspacesCommand = -- Workspaces Command
                         CreateWorkspaceCommand  Text
                         -- Navigation
                       | GotoWorkOnAWorkspace    Text
                       | QuitCommand             Text


run :: WorkOnWorkspacesStepHandle
run cliDependencies  @ Dependencies { clientDependencies} = do
  let currentStep = WorkOnWorkspacesStep run cliDependencies
  safeResponse <- liftIO $ fetchWorkspaces (read  clientDependencies)
  case safeResponse of
    Left error -> runNextStep $ Left StepError {errorDescription = show error, .. }
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
      workspaceName <- askUntil ("> enter a workspace name : " ) Nothing atLeastThreeChars

      response <- liftIO $ sendCommandAndWaitTillProcessed
                            (commandSourcer  clientDependencies)
                            CreateWorkspace {commandId ,
                                             workspaceId ,
                                             workspaceName}
      case response of
        Left  errorDescription -> return $ Left $ StepError {errorDescription = show errorDescription, ..}
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
                                            cliDependencies  @ Dependencies { clientDependencies}) = do
      displayBeginningOfACommand
      safeResponse <- liftIO $ fetchWorkspaces (read  clientDependencies)
      case safeResponse of
        Left stepError -> return $ Left StepError {errorDescription = show stepError, .. }
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

