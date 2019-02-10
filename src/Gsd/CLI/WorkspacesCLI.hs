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
import Data.Text hiding (map,foldr)
import Data.UUID.V4
import Data.UUID
import Control.Monad.IO.Class (MonadIO(liftIO))
import Gsd.Write.Client (sendCommandAndWaitResponse,SendCommandAnWaitResponse (..))
import Gsd.Write.Commands.Command
import Gsd.Read.Client (streamWorkspace )
import Gsd.Clients
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Gsd.CLI.Steps
import Servant.Client
import Data.Function ((&))
import Gsd.CLI.QuitCLI (runQuitCLI)
import qualified Servant.Client.Streaming as S
import Gsd.CLI.Greetings
import PersistedStreamEngine.Interface.PersistedItem
import Gsd.Read.Workspace
import Cqrs.Write.Aggregate.Commands.Responses.CommandResponse
import qualified Gsd.CLI.WorkspaceCLI as WorkspaceActions (run)
import Gsd.Read.GoalStats
import qualified Streamly.Safe as StreamlySafe
import Control.Exception
data WorkspacesCommand = CreateWorkspaceRequest  Text
                       | GotoWorkOnAWorkspace Text
                       | Quit Text deriving Show


run :: WorkOnWorkspacesStepHandle
run clients @ Clients {writeApiUrl,gsdReadApiUrl} = do
  let currentStep = WorkOnWorkspacesStep run clients
  manager <- liftIO $ newManager defaultManagerSettings
  result <- liftIO $ S.withClientM
                        streamWorkspace
                        (S.mkClientEnv manager gsdReadApiUrl)
                        (\result -> case result of
                             Left error -> return $ Left $ toException $ error
                             Right streamWorkspace -> do
                                 safeResponse <- (fmap.fmap) (map (\PersistedItem{item} -> item))
                                                             (streamWorkspace & StreamlySafe.toList)
                                 return safeResponse)
  case result of
      Left error -> runNextStep $ Left StepError {currentStep, errorDescription = show error }
      Right workspaces -> do

        displayWorkspacesState workspaces
        sayLn "Commands"
        let menuConfig = renderPrefixAndSuffixForDynamicGsdMenu $
                          menu (workspacesActions workspaces) stylizeAction
            prompt     = "> please choose an action (provide the index) : "
            onError    = "> please enter a valid index..."
            currentStep = WorkOnWorkspacesStep run clients

        answer <- askWithMenuRepeatedly menuConfig prompt onError
        case answer of
          CreateWorkspaceRequest description -> (runCreateWorkspaceRequest currentStep) >>= runNextStep
          GotoWorkOnAWorkspace description -> (runWorkOnAWorkspace currentStep) >>= runNextStep
          Quit description -> runQuitCLI


  where
    workspacesActions :: [Workspace] -> [WorkspacesCommand]
    workspacesActions workspaces
      | List.length workspaces == 0 = [ CreateWorkspaceRequest     "Create A Workspace" ,
                                        Quit                       "Quit" ]
      | otherwise = [ CreateWorkspaceRequest     "Create A Workspace" ,
                      GotoWorkOnAWorkspace       "Work On A Workspace",
                      Quit                       "Quit" ]

    stylizeAction :: WorkspacesCommand -> Stylized
    stylizeAction workspacesAction = case workspacesAction of
      CreateWorkspaceRequest description ->  fg white <> text description
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
      queryResult <- liftIO $ runClientM
                                (sendCommandAndWaitResponse CreateWorkspace {commandId , workspaceId , workspaceName})
                                (mkClientEnv manager writeApiUrl)
      case queryResult of
        Left errorDescription -> return $ Left $ StepError {currentStep, errorDescription = show errorDescription }
        Right RequestFailed {reason} ->  do
          sayLn $ fg red <> "The command has not been sent and taken into account : "<> (text . pack ) reason
          displayEndOfACommand
          return $ Right currentStep
        Right (CommandResponseProduced CommandFailed {reason}) ->  do
          sayLn $ fg red <> "> The command processed failed : "<> (text . pack ) reason
          displayEndOfACommand
          return $ Right currentStep
        Right ProcessMomentarilyPostponed {reason} ->  do
          sayLn $ fg red <> "> The command concumption is momentarily stopped : "<> (text . pack ) reason
          displayEndOfACommand
          return $ Right currentStep
        Right (CommandResponseProduced CommandSuccessfullyProcessed {}) ->  do
          sayLn $ fg green <> "> The command has been successfully processed... "
          displayEndOfACommand
          return $ Right currentStep


    runWorkOnAWorkspace :: Step WorkOnWorkspaces -> Byline IO (Either StepError (Step WorkOnAWorkspace))
    runWorkOnAWorkspace currentStep @ (WorkOnWorkspacesStep workOnWorkspaces clients)  = do
      displayBeginningOfACommand
      manager <- liftIO $ newManager defaultManagerSettings
      result <- liftIO $ S.withClientM
                            streamWorkspace
                            (S.mkClientEnv manager gsdReadApiUrl)
                           $ \e -> case e of
                                Left error -> return $ Left $ toException $ error
                                Right streamWorkspace -> do
                                    safeResponse <- (fmap.fmap) (map (\PersistedItem{item} -> item))
                                                                (streamWorkspace & StreamlySafe.toList)
                                    return safeResponse

      case result of
        Left stepError -> return $ Left StepError {currentStep, errorDescription = show stepError }
        Right workspaces -> do
            sayLn "Workspaces"
            let menuConfig = renderPrefixAndSuffixForDynamicGsdMenu (menu workspaces displayWorkspaceState)
                prompt     = "> please choose an action (provide the index) : "
                onError    = "> please enter a valid index..."
            workspace <- askWithMenuRepeatedly
                            menuConfig
                            prompt
                            onError
            displayEndOfACommand
            return $ Right $ WorkOnAWorkspaceStep WorkspaceActions.run clients workspace workOnWorkspaces


    displayWorkspacesState :: [Workspace] -> Byline IO ()
    displayWorkspacesState workspaces
      | (List.length workspaces) == 0 = return ()
      | otherwise =
        sayLn $
             fg white <> "Workspaces\n"
          <> (foldr (<>) "" (map (\workspace -> fg white <> "  - " <> displayWorkspaceState workspace <> "\n" ) workspaces))
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

