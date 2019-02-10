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

import Data.Text hiding (foldr,map)
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
import Data.Function ((&))
import Gsd.Read.Client (streamGoal,fetchWorkspace)
import Gsd.Read.Goal
import Gsd.CLI.Steps
import Cqrs.Write.Aggregate.Commands.Responses.CommandResponse
import Gsd.CLI.Greetings
import Gsd.Read.GoalStats
import Gsd.Read.ActionStats
import qualified  Data.List as List
import qualified Streamly.Safe as StreamlySafe
import Control.Exception

data WorkspaceCommand = RenameWorkspaceCommand       Text
                      | SetNewGoalCommand            Text
                      | ListCommandsReceived        Text
                      | ListCommandResponseProduced Text
                      | ListEventsGenerated         Text
                      | ListValidationStateHistory  Text
                      | GotoWorkOnAGoal             Text
                      | GotoWorkOnWorkspaces            Text
                      | Quit                        Text


run :: WorkOnAWorkspaceStepHandle
run clients   @ Clients {writeApiUrl,gsdMonitoringApiUrl,gsdReadApiUrl}
                workspace @ Workspace {workspaceId}
                workOnWorkspaces =  do
 let currentStep = WorkOnAWorkspaceStep run clients workspace workOnWorkspaces
 manager <- liftIO $ newManager defaultManagerSettings
 result  <- liftIO $ S.withClientM
                        (streamGoal workspaceId)
                        (S.mkClientEnv manager gsdReadApiUrl)
                        $ (\result -> case result of
                              Left error -> return $ Left $ toException $ error
                              Right stream -> do
                                 safeResponse <- stream & StreamlySafe.toList
                                 return safeResponse)
 case result of
  Left error -> runNextStep $ Left StepError {currentStep, errorDescription = show error }
  Right goals -> do
      sayLn $ displayWorkspace workspace goals
      askNextStep currentStep

 where

    askNextStep :: Step WorkOnAWorkspace -> Byline IO ()
    askNextStep currentStep = do
      sayLn "Workspace Commands"
      let menuConfig = renderPrefixAndSuffixForDynamicGsdMenu $
                          menu (workspaceActions workspace) (stylizeAction workspace)
          prompt     = "> please choose an action (provide the index) : "
          onError    = "> please enter a valid index..."


      answer <- askWithMenuRepeatedly menuConfig prompt onError
      case answer of
        RenameWorkspaceCommand description ->
          runRenameWorkspace currentStep >>= runNextStep
        SetNewGoalCommand description ->
          runSetNewGoal currentStep >>= runNextStep
        ListCommandsReceived description -> do
          result <- runListCommandReceived currentStep gsdMonitoringApiUrl workspace
          case result of
            Left stepError -> runNextStep $ Left stepError
            Right currentStep -> askNextStep currentStep
        ListCommandResponseProduced description -> do
          result <- runListCommandResponseReceived currentStep gsdMonitoringApiUrl workspace
          case result of
            Left stepError -> runNextStep $ Left stepError
            Right currentStep -> askNextStep currentStep
        ListEventsGenerated description -> do
          result <- runListEventsGenerated currentStep gsdMonitoringApiUrl workspace
          case result of
            Left stepError -> runNextStep $ Left stepError
            Right currentStep -> askNextStep currentStep
        ListValidationStateHistory description -> do
          result <- runListValidationStateHistory currentStep gsdMonitoringApiUrl workspace
          case result of
            Left stepError -> runNextStep $ Left stepError
            Right currentStep -> askNextStep currentStep
        GotoWorkOnAGoal description ->
          runWorkOnAGoal currentStep >>= runNextStep
        GotoWorkOnWorkspaces description ->
          runWorkOnWorkspaces description currentStep >>= runNextStep
        Quit description -> runQuitCLI

    workspaceActions :: Workspace -> [WorkspaceCommand]
    workspaceActions Workspace {goalStats = GoalStats {toBeAccomplished = goalsToBeAccomplished}}
      | goalsToBeAccomplished > 0 =
          [ RenameWorkspaceCommand       "Rename" ,
            SetNewGoalCommand            "Set A New Goal",
            ListCommandsReceived        "List Commands Received",
            ListCommandResponseProduced "List Command Responses Produced",
            ListEventsGenerated         "List Events Generated",
            ListValidationStateHistory  "List Validation State History",
            GotoWorkOnAGoal                 "Work On A Goal",
            GotoWorkOnWorkspaces            "Work On Another Workspace",
            Quit                        "Quit"]
      | otherwise =
                [ RenameWorkspaceCommand       "Rename" ,
                  SetNewGoalCommand            "Set A New Goal",
                  ListCommandsReceived        "List Commands Received",
                  ListCommandResponseProduced "List Command Responses Produced",
                  ListEventsGenerated         "List Events Generated",
                  ListValidationStateHistory  "List Validation State History",
                  GotoWorkOnWorkspaces         "Work On Another Workspace",
                  Quit                        "Quit"]

    stylizeAction :: Workspace -> WorkspaceCommand -> Stylized
    stylizeAction Workspace {goalStats = GoalStats {toBeAccomplished = goalsToBeAccomplished}} workspaceAction
      | goalsToBeAccomplished > 0 = case workspaceAction of
          RenameWorkspaceCommand description -> fg white <> text description
          SetNewGoalCommand description -> fg white <> text description <> "\nInfra-Monitoring"
          ListCommandsReceived description -> fg white  <>  text description
          ListCommandResponseProduced description -> fg white <> text description
          ListEventsGenerated description -> fg white <> text description
          ListValidationStateHistory description -> fg white  <> text description <> "\nNavigation"
          GotoWorkOnAGoal description -> fg white <> text description
          GotoWorkOnWorkspaces description -> fg white <> text description
          Quit description -> fg white <> text description
      | otherwise  = case workspaceAction of
          RenameWorkspaceCommand description -> fg white <> text description
          SetNewGoalCommand description -> fg white <> text description <> "\nInfra-Monitoring"
          ListCommandsReceived description -> fg white  <>  text description
          ListCommandResponseProduced description -> fg white <> text description
          ListEventsGenerated description -> fg white <> text description
          ListValidationStateHistory description -> fg white  <> text description <> "\nNavigation"
          GotoWorkOnWorkspaces description -> fg white <> text description
          Quit description -> fg white <> text description
          _ -> fg red <> "Not handle"

    runRenameWorkspace :: Step WorkOnAWorkspace -> Byline IO (Either StepError (Step WorkOnAWorkspace))
    runRenameWorkspace currentStep @ (WorkOnAWorkspaceStep
                                        workOnAWorkspace
                                        (clients @ Clients {writeApiUrl})
                                        workspace @ Workspace {workspaceId}
                                        workOnWorkspaces) = do
      commandId <- liftIO $ nextRandom
      sayLn $ fg green <> "generating a new Command Id (" <> text (toText commandId) <>") "
      workspaceNewName <- askUntil "Enter a new workspace name : " Nothing atLeastThreeChars
      manager <- liftIO $ newManager defaultManagerSettings
      queryResult <- liftIO $ runClientM (sendCommandAndWaitResponse
                                            RenameWorkspace {commandId , workspaceId, workspaceNewName})
                                         (mkClientEnv manager writeApiUrl)
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
          Right ProcessMomentarilyPostponed {reason} ->  do
            sayLn $ fg red <> "> The command concumption is momentarily stopped : "<> (text . pack ) reason
            displayEndOfACommand
            return $ Right currentStep
          Right (CommandResponseProduced CommandSuccessfullyProcessed {}) ->  do
            sayLn $ fg green <> "> The command has been successfully processed... "
            displayEndOfACommand
            liftIO $ S.withClientM
                          (fetchWorkspace workspaceId )
                          (S.mkClientEnv manager gsdReadApiUrl) $ \e -> case e of
                Left servantError -> return $ Left $ StepError {
                                              currentStep,
                                              errorDescription = show servantError }
                Right (Left applicationError)  -> return $ Left $ StepError {
                                              currentStep ,
                                              errorDescription = show applicationError}
                Right (Right Nothing)  -> return $ Left $ StepError {
                                              currentStep ,
                                              errorDescription = "Workspace asked does not exist"}
                Right (Right (Just workspace)) ->
                  return $ Right $ WorkOnAWorkspaceStep run clients workspace workOnWorkspaces



    runSetNewGoal :: Step WorkOnAWorkspace -> Byline IO (Either StepError (Step WorkOnAWorkspace))
    runSetNewGoal currentStep @ (WorkOnAWorkspaceStep
                                    workOnAWorkspace
                                    (clients @ Clients {writeApiUrl})
                                    workspace @ Workspace {workspaceId}
                                    workOnWorkspaces) = do
        commandId <- liftIO $ nextRandom
        sayLn $ fg green <> "generating a new Command Id (" <> text (toText commandId) <>") "
        goalId <- liftIO $ nextRandom
        sayLn $ fg green <> "generating a new goal Id (" <> text (toText goalId) <>") "
        goalDescription <- askUntil "Enter a goal description : " Nothing atLeastThreeChars
        manager <- liftIO $ newManager defaultManagerSettings
        queryResult <- liftIO $ runClientM (sendCommandAndWaitResponse
                                               SetGoal {commandId,
                                                        workspaceId,
                                                        goalId,
                                                        goalDescription})
                                           (mkClientEnv manager writeApiUrl)
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
            Right ProcessMomentarilyPostponed {reason} ->  do
              sayLn $ fg red <> "> The command concumption is momentarily stopped : "<> (text . pack ) reason
              displayEndOfACommand
              return $ Right currentStep
            Right (CommandResponseProduced CommandSuccessfullyProcessed {}) ->  do
              sayLn $ fg green <> "> The command has been successfully processed... "
              displayEndOfACommand
              liftIO $ S.withClientM
                                        (fetchWorkspace workspaceId )
                                        (S.mkClientEnv manager gsdReadApiUrl) $ \e -> case e of
                              Left servantError -> return $ Left $ StepError {
                                                                            currentStep,
                                                                            errorDescription = show servantError }
                              Right (Left applicationError)  -> return $ Left $ StepError {
                                                            currentStep ,
                                                            errorDescription = show applicationError}
                              Right (Right Nothing)  -> return $ Left $ StepError {
                                                            currentStep ,
                                                            errorDescription = "Workspace asked does not exist"}
                              Right (Right (Just workspace)) ->
                                return $ Right $ WorkOnAWorkspaceStep run clients workspace workOnWorkspaces



    runWorkOnAGoal :: Step WorkOnAWorkspace -> Byline IO (Either StepError (Step WorkOnAGoal))
    runWorkOnAGoal currentStep @ (WorkOnAWorkspaceStep
                                    workOnAWorkspace
                                    (clients @ Clients {gsdReadApiUrl})
                                    workspace @ Workspace {workspaceId}
                                    workOnWorkspaces) = do
     displayBeginningOfACommand
     manager <- liftIO $ newManager defaultManagerSettings
     result <-  liftIO $ S.withClientM
                          (streamGoal workspaceId)
                          (S.mkClientEnv manager gsdReadApiUrl)
                        $ (\result -> case result of
                                        Left error -> return $ Left $ toException $ error
                                        Right stream -> do
                                           safeResponse <- stream & StreamlySafe.toList
                                           return safeResponse)
     case result of
      Left stepError -> return $ Left StepError {currentStep, errorDescription = show stepError }
      Right goals -> do
         sayLn "Goals"
         let menuConfig = renderPrefixAndSuffixForDynamicGsdMenu (menu goals displayGoalForSelection)
             prompt     = "> please choose an action (provide the index) : "
             onError    = "> please enter a valid index..."
         goal <- askWithMenuRepeatedly menuConfig prompt onError
         displayEndOfACommand
         return $ Right $ WorkOnAGoalStep GoalCLI.run clients workspace goal workOnAWorkspace workOnWorkspaces

    runWorkOnWorkspaces :: Text -> Step WorkOnAWorkspace -> Byline IO (Either StepError (Step WorkOnWorkspaces))
    runWorkOnWorkspaces description (WorkOnAWorkspaceStep workOnAWorkspace clients  workspace workOnWorkspaces) = do
      sayLn $ fg green <> (text . pack .show) description <> "selected "
      return $ Right $ WorkOnWorkspacesStep workOnWorkspaces clients


    displayWorkspace :: Workspace -> [Goal] ->  Stylized
    displayWorkspace workspace @ Workspace {
                                        workspaceId,
                                        workspaceName,
                                        goalStats = GoalStats {total,toBeAccomplished,accomplished}}
                              goals =
            fg green <> "Workspaces"
         <> fg white <>" / "
         <> fg cyan <> "\""
         <> fg cyan <> text workspaceName
         <> fg cyan <> "\"\n"
         <> displayWorkspaceSummary workspace
         <> displayGoals displayGoalForWorkspaceSummary goals
         <> fg white <> "------------------------------------------"

    displayWorkspaceSummary :: Workspace -> Stylized
    displayWorkspaceSummary Workspace { goalStats = GoalStats {total,toBeAccomplished,accomplished}}
       | total == 0 = fg green <>"Summary : (No goals added in the workspace so far...)\n"
       | otherwise =
              fg white <>"Summary\n"
           <> fg white <>" - Todo : "
           <> fg green  <> (text . pack  .show) toBeAccomplished <> " goal(s) \n"
           <> fg white <>" - Done : "
           <> fg green <> (text . pack  .show) accomplished <> " goal(s)\n"
           <> fg white <>" - Given up : "
           <> fg green <> (text . pack  .show) (total - (toBeAccomplished + accomplished)) <> " goal(s)\n"

    displayGoals :: (Goal -> Stylized) -> [Goal] ->  Stylized
    displayGoals displayGoal goals
        | List.length goals == 0 = ""
        | otherwise =
               fg white <>"Goals\n"
            <> (foldr (<>) "" (map (\goal -> displayGoal goal) goals))

    displayGoalForWorkspaceSummary :: Goal -> Stylized
    displayGoalForWorkspaceSummary Goal { description , status, actionStats = ActionStats {total,completed,opened}} =
         fg white <> "  - "  <> fg green <> text description <> " : "
      <> fg white <> "status : " <> fg green <> (text . pack . show) status
      <> fg white <> ", todo : " <> fg green <> (text . pack . show) opened <> " action(s)\n"

    displayGoalForSelection :: Goal -> Stylized
    displayGoalForSelection Goal { description , status, actionStats = ActionStats {total,completed,opened}} =
          fg green <> text description <> " : "
       <> fg white <> "status : " <> fg green <> (text . pack . show) status
       <> fg white <> ", todo : " <> fg green <> (text . pack . show) opened <> " action(s)"


    atLeastThreeChars :: Text -> IO (Either Stylized Text)
    atLeastThreeChars input = return $
      if length input < 3
        then Left "3 characters minimum for a workspace please..."
        else Right input
