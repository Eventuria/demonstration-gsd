{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.CLI.WorkspaceCLI (run)where

import Prelude hiding (length)
import System.Console.Byline hiding (askWithMenuRepeatedly)
import Gsd.CLI.ByLineWrapper (askWithMenuRepeatedly,renderPrefixAndSuffixForDynamicGsdMenu)

import Data.Text
import Data.UUID.V4
import Data.UUID
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Gsd.Write.Client

import qualified Gsd.CLI.GoalCLI as GoalCLI (run)
import Gsd.CLI.WorkspaceMonitoringCLI (runListCommandReceived,
                                           runListCommandResponseReceived,
                                           runListEventsGenerated,
                                           runListValidationStateHistory)
import Gsd.CLI.QuitCLI (runQuitCLI)
import Gsd.Clients
import Servant.Client
import Gsd.Write.Commands.Command
import Gsd.Read.Workspace
import qualified Servant.Client.Streaming as S
import qualified Streamly.Prelude as Streamly.Prelude
import Streamly
import Data.Function ((&))
import Control.Monad (void)
import Gsd.Read.Client (streamGoal)
import Gsd.Read.Goal
import Gsd.CLI.Steps
import Cqrs.Write.Aggregate.Commands.Responses.CommandResponse
import Gsd.CLI.Greetings

data WorkspaceCommand = RenameWorkspaceCommand       Text
                      | SetNewGoalCommand            Text
                      | ListGoals                   Text
                      | GotoWorkOnAGoal             Text
                      | ListCommandsReceived        Text
                      | ListCommandResponseProduced Text
                      | ListEventsGenerated         Text
                      | ListValidationStateHistory  Text
                      | GotoWorkOnWorkspaces            Text
                      | Quit                        Text


run :: WorkOnAWorkspaceStepHandle
run clients   @ Clients {writeApiUrl,gsdMonitoringApiUrl,gsdReadApiUrl}
                workspace @ Workspace {workspaceId,workspaceName}
                workOnWorkspaces =  do
  let menuConfig = banner "> Available actions on the workspace " $ menu workspaceActions stylizeAction
      prompt     = "> please choose an action (provide the index) : "
      onError    = "> please enter a valid index..."
      currentStep = WorkOnAWorkspaceStep run clients workspace workOnWorkspaces

  answer <- askWithMenuRepeatedly menuConfig prompt onError
  case answer of
    RenameWorkspaceCommand description -> (runRenameWorkspace currentStep) >>= runNextStep
    SetNewGoalCommand description -> (runSetNewGoal currentStep) >>= runNextStep
    ListGoals description -> (runListGoals currentStep) >>= runNextStep
    GotoWorkOnAGoal description -> (runWorkOnAGoal currentStep) >>= runNextStep
    ListCommandsReceived description -> (runListCommandReceived currentStep gsdMonitoringApiUrl workspace) >>= runNextStep
    ListCommandResponseProduced description -> (runListCommandResponseReceived currentStep gsdMonitoringApiUrl workspace) >>= runNextStep
    ListEventsGenerated description ->         (runListEventsGenerated currentStep gsdMonitoringApiUrl workspace        ) >>= runNextStep
    ListValidationStateHistory description ->  (runListValidationStateHistory currentStep gsdMonitoringApiUrl workspace ) >>= runNextStep
    GotoWorkOnWorkspaces description -> runWorkOnWorkspaces description currentStep >>= runNextStep
    Quit description -> runQuitCLI

  where
    workspaceActions :: [WorkspaceCommand]
    workspaceActions =
      [ RenameWorkspaceCommand       "Rename Workspace" ,
        SetNewGoalCommand            "Set A New Goal",
        ListGoals                   "List Goals",
        GotoWorkOnAGoal                 "Work On A Goal",
        ListCommandsReceived        "List Commands Received",
        ListCommandResponseProduced "List Command Responses Produced",
        ListEventsGenerated         "List Events Generated",
        ListValidationStateHistory  "List Validation State History",
        GotoWorkOnWorkspaces            "Work On Another Workspace",
        Quit                        "Quit"]

    stylizeAction :: WorkspaceCommand -> Stylized
    stylizeAction workspaceAction = case workspaceAction of
      RenameWorkspaceCommand description -> fg white <> text description
      SetNewGoalCommand description -> fg white <> text description
      ListGoals description -> fg white <> text description
      GotoWorkOnAGoal description -> fg white <> text description <> "\n\n> Infra-Monitoring \n"
      ListCommandsReceived description -> fg white <> text description
      ListCommandResponseProduced description -> fg white <> text description
      ListEventsGenerated description -> fg white <> text description
      ListValidationStateHistory description -> fg white <> text description <> "\n\n> Navigation \n"
      GotoWorkOnWorkspaces description -> fg white <> text description
      Quit description -> fg white <> text description

runRenameWorkspace :: Step WorkOnAWorkspace -> Byline IO (Either StepError (Step WorkOnAWorkspace))
runRenameWorkspace currentStep @ (WorkOnAWorkspaceStep workOnAWorkspace (clients @ Clients {writeApiUrl}) (workspace @ Workspace {workspaceId,..}) workOnWorkspaces) = do
  commandId <- liftIO $ nextRandom
  sayLn $ fg green <> "generating a new Command Id (" <> text (toText commandId) <>") "
  workspaceNewName <- askUntil "Enter a new workspace name : " Nothing atLeastThreeChars
  manager <- liftIO $ newManager defaultManagerSettings
  queryResult <- liftIO $ runClientM (sendCommandAndWaitResponse RenameWorkspace {commandId , workspaceId = workspaceId , workspaceNewName}) (mkClientEnv manager writeApiUrl)
  case queryResult of
      Left  errorDescription -> return $ Left $ StepError {currentStep , errorDescription = show errorDescription}
      Right RequestFailed {reason} ->  do
        sayLn $ fg red <> "The command has not been sent and taken into account : "<> (text . pack ) reason
        displayEndOfACommand
        return $ Right currentStep
      Right (CommandResponseProduced CommandFailed {reason}) ->  do
        sayLn $ fg red <> "> The command processed failed : "<> (text . pack ) reason
        displayEndOfACommand
        return $ Right currentStep
      Right (CommandResponseProduced CommandSuccessfullyProcessed {}) ->  do
        sayLn $ fg green <> "> The command has been successfully processed... "
        displayEndOfACommand
        return $ Right currentStep


runSetNewGoal :: Step WorkOnAWorkspace -> Byline IO (Either StepError (Step WorkOnAWorkspace))
runSetNewGoal currentStep @ (WorkOnAWorkspaceStep workOnAWorkspace (clients @ Clients {writeApiUrl}) (workspace @ Workspace {workspaceId}) workOnWorkspaces) = do
    commandId <- liftIO $ nextRandom
    sayLn $ fg green <> "generating a new Command Id (" <> text (toText commandId) <>") "
    goalId <- liftIO $ nextRandom
    sayLn $ fg green <> "generating a new goal Id (" <> text (toText goalId) <>") "
    goalDescription <- askUntil "Enter a goal description : " Nothing atLeastThreeChars
    manager <- liftIO $ newManager defaultManagerSettings
    queryResult <- liftIO $ runClientM (sendCommandAndWaitResponse SetGoal {commandId , workspaceId = workspaceId , goalId, goalDescription}) (mkClientEnv manager writeApiUrl)
    case queryResult of
        Left  errorDescription -> return $ Left $ StepError {currentStep , errorDescription = show errorDescription}
        Right RequestFailed {reason} ->  do
          sayLn $ fg red <> "The command has not been sent and taken into account : "<> (text . pack ) reason
          displayEndOfACommand
          return $ Right currentStep
        Right (CommandResponseProduced CommandFailed {reason}) ->  do
          sayLn $ fg red <> "> The command processed failed : "<> (text . pack ) reason
          displayEndOfACommand
          return $ Right currentStep
        Right (CommandResponseProduced CommandSuccessfullyProcessed {}) ->  do
          sayLn $ fg green <> "> The command has been successfully processed... "
          displayEndOfACommand
          return $ Right currentStep


runListGoals :: Step WorkOnAWorkspace -> Byline IO (Either StepError (Step WorkOnAWorkspace))
runListGoals currentStep @ (WorkOnAWorkspaceStep workOnAWorkspace (clients @ Clients {gsdReadApiUrl}) (workspace @ Workspace {workspaceId,workspaceName}) workOnWorkspaces) = do
  sayLn $ fg green <> "Listing goals set : "
  manager <- liftIO $ newManager defaultManagerSettings
  liftIO $ S.withClientM (streamGoal workspaceId)(S.mkClientEnv manager gsdReadApiUrl) $ \e -> case e of
      Left errorDescription -> return $ Left $ StepError {currentStep,errorDescription = show errorDescription }
      Right stream -> do
          runStream $ stream
              & Streamly.Prelude.mapM (\goal -> void $ runByline $ do
                sayLn $ fg green <> (text . pack . show) goal)
          return $ Right currentStep

runWorkOnAGoal :: Step WorkOnAWorkspace -> Byline IO (Either StepError (Step WorkOnAGoal))
runWorkOnAGoal currentStep @ (WorkOnAWorkspaceStep workOnAWorkspace (clients @ Clients {gsdReadApiUrl}) (workspace @ Workspace {workspaceId,workspaceName}) workOnWorkspaces) = do
 manager <- liftIO $ newManager defaultManagerSettings
 result <-  liftIO $ S.withClientM (streamGoal workspaceId) (S.mkClientEnv manager gsdReadApiUrl) $ \e -> case e of
               Left errorDescription -> return $ Left $ StepError {currentStep,errorDescription = show errorDescription }
               Right stream -> do
                 goals <- stream & Streamly.Prelude.toList
                 return $ Right goals
 case result of
  Left stepError -> return $ Left stepError
  Right goals -> do
     let menuConfig = banner "> available Goals " $ renderPrefixAndSuffixForDynamicGsdMenu (menu goals stylizeGoal)
         prompt     = "> please choose an action (provide the index) : "
         onError    = "> please enter a valid index..."
     goal <- askWithMenuRepeatedly menuConfig prompt onError

     return $ Right $ WorkOnAGoalStep GoalCLI.run clients workspace goal workOnAWorkspace workOnWorkspaces

runWorkOnWorkspaces :: Text -> Step WorkOnAWorkspace -> Byline IO (Either StepError (Step WorkOnWorkspaces))
runWorkOnWorkspaces description (WorkOnAWorkspaceStep workOnAWorkspace clients  workspace workOnWorkspaces) = do
  sayLn $ fg green <> (text . pack .show) description <> "selected "
  return $ Right $ WorkOnWorkspacesStep workOnWorkspaces clients


stylizeGoal :: Goal -> Stylized
stylizeGoal Goal {goalId,description} =
  fg cyan <> "Goal (" <> (text $ toText goalId) <> " , " <> text description <> " )"


atLeastThreeChars :: Text -> IO (Either Stylized Text)
atLeastThreeChars input = return $
  if length input < 3
    then Left "3 characters minimum for a workspace please..."
    else Right input
