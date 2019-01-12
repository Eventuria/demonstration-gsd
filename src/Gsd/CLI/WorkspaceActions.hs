{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Gsd.CLI.WorkspaceActions (run)where

import Prelude hiding (length)
import System.Console.Byline hiding (askWithMenuRepeatedly)
import Gsd.CLI.ByLineWrapper (askWithMenuRepeatedly)

import Data.Text
import Data.UUID.V4
import Data.UUID
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Gsd.Write.Client (sendCommand)

import qualified Gsd.CLI.GoalActions as GoalActions (run)
import Gsd.CLI.WorkspaceMonitoringActions (runListCommandReceived,
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


data WorkspaceAction =  RenameWorkspaceAction       Text
                      | SetNewGoalAction            Text
                      | ListGoals                   Text
                      | GotoWorkOnAGoal                 Text
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
  let menuConfig = banner ("Available actions on the selected workspace : " <> fg green <> ((text . pack .show) $ workspaceName)) $ menu workspaceActions stylizeAction
      prompt     = "please choose an action (provide the index) : "
      onError    = "please enter a valid index..."
      currentStep = WorkOnAWorkspaceStep run clients workspace workOnWorkspaces

  answer <- askWithMenuRepeatedly menuConfig prompt onError
  case answer of
      Right (RenameWorkspaceAction description) -> (runRenameWorkspace currentStep) >>= runNextStep
      Right (SetNewGoalAction description) -> (runSetNewGoal currentStep) >>= runNextStep
      Right (ListGoals description) -> (runListGoals currentStep) >>= runNextStep
      Right (GotoWorkOnAGoal description) -> (runWorkOnAGoal currentStep) >>= runNextStep
      Right (ListCommandsReceived description) -> (runListCommandReceived currentStep gsdMonitoringApiUrl workspace) >>= runNextStep 
      Right (ListCommandResponseProduced description) -> (runListCommandResponseReceived currentStep gsdMonitoringApiUrl workspace) >>= runNextStep
      Right (ListEventsGenerated description) ->         (runListEventsGenerated currentStep gsdMonitoringApiUrl workspace        ) >>= runNextStep
      Right (ListValidationStateHistory description) ->  (runListValidationStateHistory currentStep gsdMonitoringApiUrl workspace ) >>= runNextStep
      Right (GotoWorkOnWorkspaces description) -> runWorkOnWorkspaces description currentStep >>= runNextStep
      Right (Quit description) -> runQuitCLI
      Left  error -> (return $ Left StepError {currentStep, errorDescription = show error }) >>= runNextStep

  where
    workspaceActions :: [WorkspaceAction]
    workspaceActions =
      [ RenameWorkspaceAction       "Rename Workspace" ,
        SetNewGoalAction            "Set A New Goal",
        ListGoals                   "List Goals",
        GotoWorkOnAGoal                 "Work On A Goal",
        GotoWorkOnWorkspaces            "Work On Another Workspace",
        ListCommandsReceived        "Infra-Monitoring - List Commands Received",
        ListCommandResponseProduced "Infra-Monitoring - List Command Responses Produced",
        ListEventsGenerated         "Infra-Monitoring - List Events Generated",
        ListValidationStateHistory  "Infra-Monitoring - List Validation State History",
        Quit                        "Quit"]

    stylizeAction :: WorkspaceAction -> Stylized
    stylizeAction workspaceAction = case workspaceAction of
      RenameWorkspaceAction description -> fg cyan <> text description
      SetNewGoalAction description -> fg cyan <> text description
      ListGoals description -> fg cyan <> text description
      GotoWorkOnAGoal description -> fg cyan <> text description
      ListCommandsReceived description -> fg cyan <> text description
      ListCommandResponseProduced description -> fg cyan <> text description
      ListEventsGenerated description -> fg cyan <> text description
      ListValidationStateHistory description -> fg cyan <> text description
      GotoWorkOnWorkspaces description -> fg cyan <> text description
      Quit description -> fg cyan <> text description

runRenameWorkspace :: Step WorkOnAWorkspace -> Byline IO (Either StepError (Step WorkOnAWorkspace))
runRenameWorkspace currentStep @ (WorkOnAWorkspaceStep workOnAWorkspace (clients @ Clients {writeApiUrl}) (workspace @ Workspace {workspaceId}) workOnWorkspaces) = do
  commandId <- liftIO $ nextRandom
  sayLn $ fg green <> "generating a new Command Id (" <> text (toText commandId) <>") "
  workspaceNewName <- askUntil "Enter a new workspace name : " Nothing atLeastThreeChars
  manager <- liftIO $ newManager defaultManagerSettings
  queryResult <- liftIO $ runClientM (sendCommand RenameWorkspace {commandId , workspaceId = workspaceId , workspaceNewName}) (mkClientEnv manager writeApiUrl)
  case queryResult of
      Left  errorDescription -> return $ Left $ StepError {currentStep , errorDescription = show errorDescription}
      Right persistenceResult -> do
        sayLn $ fg green <> "Rename Workspace Command successfully sent !"
        sayLn $ ""
        return $ Right $ WorkOnAWorkspaceStep workOnAWorkspace clients Workspace {workspaceId,workspaceName = workspaceNewName} workOnWorkspaces


runSetNewGoal :: Step WorkOnAWorkspace -> Byline IO (Either StepError (Step WorkOnAWorkspace))
runSetNewGoal currentStep @ (WorkOnAWorkspaceStep workOnAWorkspace (clients @ Clients {writeApiUrl}) (workspace @ Workspace {workspaceId}) workOnWorkspaces) = do
    commandId <- liftIO $ nextRandom
    sayLn $ fg green <> "generating a new Command Id (" <> text (toText commandId) <>") "
    goalId <- liftIO $ nextRandom
    sayLn $ fg green <> "generating a new goal Id (" <> text (toText goalId) <>") "
    goalDescription <- askUntil "Enter a goal description : " Nothing atLeastThreeChars
    manager <- liftIO $ newManager defaultManagerSettings
    queryResult <- liftIO $ runClientM (sendCommand SetGoal {commandId , workspaceId = workspaceId , goalId, goalDescription}) (mkClientEnv manager writeApiUrl)
    case queryResult of
        Left errorDescription -> return $ Left $ StepError {currentStep,errorDescription = show errorDescription }
        Right persistenceResult -> do
          sayLn $ fg green <> "Set Goal Command successfully sent !"
          sayLn $ ""
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
     let menuConfig = banner "Available Goals :" $ menu goals stylizeGoal
         prompt     = "please choose an action (provide the index) : "
         onError    = "please enter a valid index..."
     result <- askWithMenuRepeatedly menuConfig prompt onError
     case result of
       Left errorDescription -> return $ Left $ StepError {currentStep,errorDescription = show errorDescription }
       Right goal -> do
         sayLn $ fg green <> (text . pack . show) goal <> " selected !"
         return $ Right $ WorkOnAGoalStep GoalActions.run clients workspace goal workOnAWorkspace workOnWorkspaces

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
