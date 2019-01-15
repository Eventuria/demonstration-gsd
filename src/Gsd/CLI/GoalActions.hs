{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Gsd.CLI.GoalActions (run)where

import Prelude hiding (length)

import Gsd.CLI.BreadCrumbs (breadCrumb)
import Data.Text
import Data.UUID.V4
import Data.UUID
import Control.Monad.IO.Class (MonadIO(liftIO))
import System.Console.Byline hiding (askWithMenuRepeatedly)
import Gsd.CLI.ByLineWrapper (askWithMenuRepeatedly)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Gsd.Write.Client (sendCommand)
import Gsd.CLI.QuitCLI (runQuitCLI)
import Servant.Client
import Gsd.Clients
import Gsd.Read.Workspace
import Gsd.Read.Goal
import Gsd.CLI.Steps
import Gsd.Write.Commands.Command
import Cqrs.Write.Aggregate.Commands.CommandId
import Gsd.Write.Core

data GoalActions =  RefineGoalDescriptionAction Text
                  | ChangeGoalStatus            Text
                  | GotoWorkOnWorkspace         Text
                  | GotoWorkOnWorkspaces        Text
                  | Quit                        Text




run :: WorkOnAGoalStepHandle
run clients   @ Clients {writeApiUrl,gsdMonitoringApiUrl,gsdReadApiUrl}
           workspace @ Workspace {workspaceId,workspaceName}
           goal      @ Goal {goalId,description,status = currentStatus}
           workOnWorkspace
           workOnWorkspaces = do
  sayLn $ breadCrumb workspace goal
  sayLn $ fg white <> "Current Status : " <> fg green <> (text . pack .show) currentStatus
  let menuConfig = banner ("Available actions on the selected goal : " <> fg green <> ((text . pack .show) $ description)) $ menu workspaceActions stylizeAction
      prompt     = "please choose an action (provide the index) : "
      onError    = "please enter a valid index..."
      currentStep = WorkOnAGoalStep run clients workspace goal workOnWorkspace workOnWorkspaces

  answer <- askWithMenuRepeatedly menuConfig prompt onError
  case answer of
    RefineGoalDescriptionAction description -> (runRefineGoalDescriptionAction currentStep) >>= runNextStep
    ChangeGoalStatus description -> runChangeGoalStatus currentStep >>= runNextStep
    GotoWorkOnWorkspace description -> runWorkOnWorkspace description currentStep >>= runNextStep
    GotoWorkOnWorkspaces description -> runWorkOnWorkspaces description currentStep >>= runNextStep
    Quit description -> runQuitCLI

  where

    workspaceActions :: [GoalActions]
    workspaceActions =
      [ RefineGoalDescriptionAction  "Refine the goal description" ,
        ChangeGoalStatus             "Change the status",
        GotoWorkOnWorkspace          "Come Back To The Workspace  actions",
        GotoWorkOnWorkspaces         "Come Back To The Workspaces actions",
        Quit                         "Quit"]

    stylizeAction :: GoalActions -> Stylized
    stylizeAction (RefineGoalDescriptionAction description) = fg cyan <> text description
    stylizeAction (ChangeGoalStatus description) = fg cyan <> text description
    stylizeAction (GotoWorkOnWorkspace description) = fg cyan <> text description
    stylizeAction (GotoWorkOnWorkspaces description) = fg cyan <> text description
    stylizeAction (Quit description) = fg cyan <> text description


runWorkOnWorkspaces :: Text -> Step WorkOnAGoal -> Byline IO (Either StepError (Step WorkOnWorkspaces))
runWorkOnWorkspaces description currentStep @ (WorkOnAGoalStep workOnAGoalStepHandle clients workspace goal workOnWorkspace workOnWorkspaces) = do
  sayLn $ fg green <> (text . pack .show) description <> "selected "
  return $ Right $ WorkOnWorkspacesStep workOnWorkspaces clients

runWorkOnWorkspace :: Text -> Step WorkOnAGoal -> Byline IO (Either StepError (Step WorkOnAWorkspace))
runWorkOnWorkspace description currentStep @ (WorkOnAGoalStep workOnAGoalStepHandle clients workspace goal workOnWorkspace workOnWorkspaces) = do
  sayLn $ fg green <> (text . pack .show) description <> "selected "
  return $ Right $ WorkOnAWorkspaceStep workOnWorkspace  clients workspace workOnWorkspaces

runRefineGoalDescriptionAction :: Step WorkOnAGoal -> Byline IO (Either StepError (Step WorkOnAGoal))
runRefineGoalDescriptionAction currentStep @ (WorkOnAGoalStep workOnAGoalStepHandle clients @ Clients {writeApiUrl} workspace goal @Goal {workspaceId,goalId,status} workOnWorkspace workOnWorkspaces) = do
  commandId <- liftIO $ nextRandom
  sayLn $ fg green <> "generating a new Command Id (" <> text (toText commandId) <>") "
  refinedGoalDescription <- askUntil "Enter a new goal description : " Nothing atLeastThreeChars
  manager  <- liftIO $ newManager defaultManagerSettings
  queryResult <- liftIO $ runClientM (sendCommand RefineGoalDescription {commandId , workspaceId , goalId, refinedGoalDescription}) (mkClientEnv manager writeApiUrl)
  case queryResult of
      Left  errorDescription -> return $ Left $ StepError {currentStep , errorDescription = show errorDescription}
      Right persistenceResult -> do
        sayLn $ fg green <> "Rename Workspace Command successfully sent !"
        sayLn $ ""
        return $ Right $ WorkOnAGoalStep run clients workspace Goal {workspaceId,goalId, description = refinedGoalDescription,status} workOnWorkspace workOnWorkspaces



runChangeGoalStatus :: Step WorkOnAGoal -> Byline IO (Either StepError (Step WorkOnAGoal))
runChangeGoalStatus currentStep @ (WorkOnAGoalStep workOnAGoalStepHandle clients @ Clients {writeApiUrl} workspace goal @Goal {workspaceId,goalId,status = currentStatus,description} workOnWorkspace workOnWorkspaces) = do

  case (getNextStatusAvailable currentStatus) of
     [] -> do
        sayLn $ fg green <> "No status avalaible (final state)"
        return $ Right currentStep
     getNextStatusAvailable -> do
        let menuConfig = banner ("Available status on the selected goal : ") $ menu getNextStatusAvailable (\status -> fg cyan <> (text.pack.show) status)
            prompt     = "please choose an action (provide the index) : "
            onError    = "please enter a valid index..."

        nextStatus <- askWithMenuRepeatedly menuConfig prompt onError
        commandId <- liftIO $ nextRandom
        sayLn $ fg green <> "generating a new Command Id (" <> text (toText commandId) <>") "

        commandToSent <- getCommandToSend nextStatus commandId goalId workspaceId

        manager <- liftIO $ newManager defaultManagerSettings
        queryResult <- liftIO $ runClientM (sendCommand commandToSent) (mkClientEnv manager writeApiUrl)
        case queryResult of
            Left errorDescription -> return $ Left $ StepError {currentStep,errorDescription = show errorDescription }
            Right persistenceResult -> do
              sayLn $ fg green <> "Command successfully sent !"
              sayLn $ ""
              return $ Right (WorkOnAGoalStep workOnAGoalStepHandle clients workspace Goal {workspaceId,goalId,status = nextStatus, description} workOnWorkspace workOnWorkspaces)

  where
      getCommandToSend :: GoalStatus -> CommandId -> GoalId -> WorkspaceId -> Byline IO (GsdCommand)
      getCommandToSend goalStatus commandId goalId workspaceId = case goalStatus of
        Created -> error "Can't have Created as the next step"
        InProgress -> return $ StartWorkingOnGoal {commandId,goalId,workspaceId}
        Paused -> return $ PauseWorkingOnGoal {commandId,goalId,workspaceId}
        Accomplished -> return $ NotifyGoalAccomplishment {commandId,goalId,workspaceId}
        GivenUp -> do
            reason <- askUntil "Enter a reason why you give up on that goal : " Nothing atLeastThreeChars
            return $ GiveUpOnGoal {commandId,goalId,workspaceId,reason}


atLeastThreeChars :: Text -> IO (Either Stylized Text)
atLeastThreeChars input = return $
  if length input < 3
    then Left "3 characters minimum for a workspace please..."
    else Right input
