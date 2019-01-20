{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Gsd.CLI.GoalCLI (run)where

import Prelude hiding (length)
import Data.Function ((&))
import Control.Monad (void)
import qualified Servant.Client.Streaming as S
import qualified Streamly.Prelude as Streamly.Prelude
import Streamly
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
import Gsd.Read.Client (streamAction)
import Gsd.Read.Workspace
import Gsd.Read.Goal
import Gsd.Read.Action
import Gsd.CLI.Steps
import Gsd.Write.Commands.Command
import Cqrs.Write.Aggregate.Commands.CommandId
import Gsd.Write.Core

data GoalCommands =  RefineGoalDescriptionCommand Text
                  | ChangeGoalStatus              Text
                  | ActionizeOnGoalCommand        Text
                  | ListGoalActions             Text
                  | NotifyGoalAccomplishmentCommand  Text
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
      prompt     = "please choose a command (provide the index) : "
      onError    = "please enter a valid index..."
      currentStep = WorkOnAGoalStep run clients workspace goal workOnWorkspace workOnWorkspaces

  answer <- askWithMenuRepeatedly menuConfig prompt onError
  case answer of
    RefineGoalDescriptionCommand description -> (runRefineGoalDescriptionCommand currentStep) >>= runNextStep
    ChangeGoalStatus description -> runChangeGoalStatus currentStep >>= runNextStep
    ActionizeOnGoalCommand description -> runActionizeOnGoalCommand currentStep >>= runNextStep
    ListGoalActions description -> runListGoalActions currentStep >>= runNextStep
    NotifyGoalAccomplishmentCommand description -> runNotifyGoalAccomplishmentCommand currentStep >>= runNextStep
    GotoWorkOnWorkspace description -> runWorkOnWorkspace description currentStep >>= runNextStep
    GotoWorkOnWorkspaces description -> runWorkOnWorkspaces description currentStep >>= runNextStep
    Quit description -> runQuitCLI

  where

    workspaceActions :: [GoalCommands]
    workspaceActions =
      [ RefineGoalDescriptionCommand  "Refine The Goal Description" ,
        ChangeGoalStatus             "Change The Status",
        ActionizeOnGoalCommand       "Define A New Action On This Goal",
        ListGoalActions              "List Actions",
        NotifyGoalAccomplishmentCommand "Notify The Accomplishement Of An Action",
        GotoWorkOnWorkspace          "Come Back To The Workspace  Commands",
        GotoWorkOnWorkspaces         "Come Back To The Workspaces Commands",
        Quit                         "Quit"]

    stylizeAction :: GoalCommands -> Stylized
    stylizeAction (RefineGoalDescriptionCommand description) = fg cyan <> text description
    stylizeAction (ChangeGoalStatus description) = fg cyan <> text description
    stylizeAction (ListGoalActions description) = fg cyan <> text description
    stylizeAction (ActionizeOnGoalCommand description) = fg cyan <> text description
    stylizeAction (NotifyGoalAccomplishmentCommand description) = fg cyan <> text description
    stylizeAction (GotoWorkOnWorkspace description) = fg cyan <> text description
    stylizeAction (GotoWorkOnWorkspaces description) = fg cyan <> text description
    stylizeAction (Quit description) = fg cyan <> text description


runWorkOnWorkspaces :: Text -> Step WorkOnAGoal -> Byline IO (Either StepError (Step WorkOnWorkspaces))
runWorkOnWorkspaces description currentStep @ (WorkOnAGoalStep workOnAGoalStepHandle clients workspace goal workOnWorkspace workOnWorkspaces) = do
  sayLn $ fg green <> (text . pack .show) description <> "selected "
  return $ Right $ WorkOnWorkspacesStep workOnWorkspaces clients


runNotifyGoalAccomplishmentCommand :: Step WorkOnAGoal -> Byline IO (Either StepError (Step WorkOnAGoal))
runNotifyGoalAccomplishmentCommand currentStep @ (WorkOnAGoalStep workOnAGoalStepHandle clients @ Clients {writeApiUrl,gsdReadApiUrl} workspace goal @Goal {workspaceId,goalId,status = currentStatus,description} workOnWorkspace workOnWorkspaces) = do
  manager <- liftIO $ newManager defaultManagerSettings
  result <-  liftIO $ S.withClientM (streamAction workspaceId goalId) (S.mkClientEnv manager gsdReadApiUrl) $ \e -> case e of
               Left errorDescription -> return $ Left $ StepError {currentStep,errorDescription = show errorDescription }
               Right stream -> do
                 actions <- stream & Streamly.Prelude.toList
                 return $ Right actions
  case result of
    Left stepError -> return $ Left stepError
    Right actions -> do
       let menuConfig = banner "Available actions :" $ menu actions stylizeAction
           prompt     = "please choose the action you consider Accomplished (provide the index) : "
           onError    = "please enter a valid index..."
       Action {actionId} <- askWithMenuRepeatedly menuConfig prompt onError
       commandId <- liftIO $ nextRandom
       sayLn $ fg green <> "generated a new Command Id (" <> text (toText commandId) <>") "
       queryResult <- liftIO $ runClientM (sendCommand (NotifyActionCompleted {commandId , workspaceId , goalId, actionId})) (mkClientEnv manager writeApiUrl)
       case queryResult of
           Left  errorDescription -> return $ Left $ StepError {currentStep , errorDescription = show errorDescription}
           Right persistenceResult -> do
             sayLn $ fg green <> "Command successfully sent !"
             sayLn $ ""
             return $ Right $ currentStep
    where
      stylizeAction :: Action -> Stylized
      stylizeAction Action {indexation,details} =
        fg cyan <> "Action (" <> (text $ (pack.show) indexation) <> " , " <> text details <> " )"



runWorkOnWorkspace :: Text -> Step WorkOnAGoal -> Byline IO (Either StepError (Step WorkOnAWorkspace))
runWorkOnWorkspace description currentStep @ (WorkOnAGoalStep workOnAGoalStepHandle clients workspace goal workOnWorkspace workOnWorkspaces) = do
  sayLn $ fg green <> (text . pack .show) description <> "selected "
  return $ Right $ WorkOnAWorkspaceStep workOnWorkspace  clients workspace workOnWorkspaces

runRefineGoalDescriptionCommand :: Step WorkOnAGoal -> Byline IO (Either StepError (Step WorkOnAGoal))
runRefineGoalDescriptionCommand currentStep @ (WorkOnAGoalStep workOnAGoalStepHandle clients @ Clients {writeApiUrl} workspace goal @Goal {workspaceId,goalId,status} workOnWorkspace workOnWorkspaces) = do
  commandId <- liftIO $ nextRandom
  sayLn $ fg green <> "generated a new Command Id (" <> text (toText commandId) <>") "
  refinedGoalDescription <- askUntil "Enter a new goal description : " Nothing atLeastThreeChars
  manager  <- liftIO $ newManager defaultManagerSettings
  queryResult <- liftIO $ runClientM (sendCommand RefineGoalDescription {commandId , workspaceId , goalId, refinedGoalDescription}) (mkClientEnv manager writeApiUrl)
  case queryResult of
      Left  errorDescription -> return $ Left $ StepError {currentStep , errorDescription = show errorDescription}
      Right persistenceResult -> do
        sayLn $ fg green <> "Command successfully sent !"
        sayLn $ ""
        return $ Right $ WorkOnAGoalStep run clients workspace Goal {workspaceId,goalId, description = refinedGoalDescription,status} workOnWorkspace workOnWorkspaces


runActionizeOnGoalCommand :: Step WorkOnAGoal -> Byline IO (Either StepError (Step WorkOnAGoal))
runActionizeOnGoalCommand currentStep @ (WorkOnAGoalStep workOnAGoalStepHandle clients @ Clients {writeApiUrl} workspace goal @Goal {workspaceId,goalId,status} workOnWorkspace workOnWorkspaces) = do
  commandId <- liftIO $ nextRandom
  sayLn $ fg green <> "generated a new Command Id (" <> text (toText commandId) <>") "
  actionId <- liftIO $ nextRandom
  sayLn $ fg green <> "generated a new Action Id (" <> text (toText commandId) <>") "
  actionDetails <- askUntil "Enter the details of the action : " Nothing atLeastThreeChars
  manager  <- liftIO $ newManager defaultManagerSettings
  queryResult <- liftIO $ runClientM (sendCommand ActionizeOnGoal {commandId , workspaceId , goalId, actionId , actionDetails}) (mkClientEnv manager writeApiUrl)
  case queryResult of
      Left  errorDescription -> return $ Left $ StepError {currentStep , errorDescription = show errorDescription}
      Right persistenceResult -> do
        sayLn $ fg green <> "Command successfully sent !"
        sayLn $ ""
        return $ Right currentStep

runListGoalActions :: Step WorkOnAGoal -> Byline IO (Either StepError (Step WorkOnAGoal))
runListGoalActions currentStep @ (WorkOnAGoalStep workOnAGoalStepHandle clients @ Clients {gsdReadApiUrl} workspace goal @Goal {workspaceId,goalId,status = currentStatus,description} workOnWorkspace workOnWorkspaces) = do
  sayLn $ fg green <> "Listing actions set : "
  manager <- liftIO $ newManager defaultManagerSettings
  liftIO $ S.withClientM (streamAction workspaceId goalId)(S.mkClientEnv manager gsdReadApiUrl) $ \e -> case e of
      Left errorDescription -> return $ Left $ StepError {currentStep,errorDescription = show errorDescription }
      Right stream -> do
          runStream $ stream & Streamly.Prelude.mapM (\action -> void $ runByline $ do displayAction action )
          return $ Right currentStep
  where
    displayAction :: Action -> Byline IO ()
    displayAction Action {indexation, details, status} = do
      sayLn $ fg green <> "\t" <> (text . pack . show) indexation <> ") " <> text details <> "( " <> (text . pack . show) status <> " )"



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
        sayLn $ fg green <> "generated a new Command Id (" <> text (toText commandId) <>") "

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


