{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.GSD.CLI.UI.Workspace (run)where

import           Prelude hiding (length,read)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Text hiding (foldr,map)
import           Data.UUID.V4
import           Data.UUID
import           Data.Functor
import qualified Data.List                       as List

import           System.Console.Byline hiding (askWithMenuRepeatedly)

import           Eventuria.Adapters.ByLine.Wrapper (askWithMenuRepeatedly,renderPrefixAndSuffixForDynamicGsdMenu)

import           Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Responses.CommandResponse

import qualified Eventuria.GSD.CLI.UI.Goal       as GoalCLI (run)
import qualified Eventuria.GSD.CLI.UI.Monitoring as MonitoringCLI

import           Eventuria.GSD.Write.CommandSourcer.Client.Client
import           Eventuria.GSD.CLI.UI.Monitoring (runMonitoringCommand)
import           Eventuria.GSD.CLI.UI.Quit (runQuitCLI)
import           Eventuria.GSD.CLI.Dependencies
import           Eventuria.GSD.Write.Model.Commands.Command
import           Eventuria.GSD.Read.Model.Workspace
import           Eventuria.GSD.Read.API.Client.Client (fetchGoals,fetchWorkspace)
import           Eventuria.GSD.Read.Model.Goal
import           Eventuria.GSD.CLI.Workflow.Steps
import           Eventuria.GSD.CLI.UI.Greetings
import           Eventuria.GSD.Read.Model.GoalStats
import           Eventuria.GSD.Read.Model.ActionStats


data WorkspaceCommand = -- Workspace Commands
                        RenameWorkspaceCommand       Text
                      | SetNewGoalCommand            Text
                        -- Infra-Monitoring
                      | ListCommandsReceived         Text
                      | ListCommandResponsesProduced Text
                      | ListEventsGenerated          Text
                      | ListValidationStates         Text
                        -- Navigation
                      | GotoWorkOnAGoal              Text
                      | GotoWorkOnWorkspaces         Text
                      | QuitCommand                  Text


run :: WorkOnAWorkspaceStepHandle
run cliDependencies  @ Dependencies { clientDependencies}
    workspace @ Workspace {workspaceId}
    workOnWorkspaces =  do
 let currentStep = WorkOnAWorkspaceStep run cliDependencies workspace workOnWorkspaces
 safeResponse  <- liftIO $ fetchGoals (read  clientDependencies) workspaceId
 case safeResponse of
  Left error -> runNextStep $ Left StepError {currentStep, errorDescription = show error }
  Right goals -> do
      sayLn $ displayWorkspace workspace goals
      proposeAvailableWorkspaceCommands

 where

    proposeAvailableWorkspaceCommands :: Byline IO ()
    proposeAvailableWorkspaceCommands = do
      sayLn "Workspace Commands"
      let menuConfig = renderPrefixAndSuffixForDynamicGsdMenu $
                          menu (workspaceActions workspace) (stylizeAction workspace)
          prompt     = "> please choose an action (provide the index) : "
          onError    = "> please enter a valid index..."
          currentStep = WorkOnAWorkspaceStep run cliDependencies workspace workOnWorkspaces

      answer <- askWithMenuRepeatedly menuConfig prompt onError
      case answer of
        RenameWorkspaceCommand        _ -> runRenameWorkspace   currentStep >>= runNextStep
        SetNewGoalCommand             _ -> runSetNewGoal        currentStep >>= runNextStep
        ListCommandsReceived          _ -> runMonitoringCommand
                                            currentStep
                                            MonitoringCLI.ListCommandsReceived
                                            (monitoring clientDependencies)
                                            workspace >>= runNextStepOnErrorOrProposeAvailableWorkspaceCommandAgain
        ListCommandResponsesProduced  _ -> runMonitoringCommand currentStep
                                            MonitoringCLI.ListCommandResponsesProduced
                                            (monitoring clientDependencies)
                                            workspace >>= runNextStepOnErrorOrProposeAvailableWorkspaceCommandAgain
        ListEventsGenerated           _ -> runMonitoringCommand currentStep
                                            MonitoringCLI.ListEventsGenerated
                                            (monitoring clientDependencies)
                                            workspace >>= runNextStepOnErrorOrProposeAvailableWorkspaceCommandAgain
        ListValidationStates          _ -> runMonitoringCommand currentStep
                                            MonitoringCLI.ListValidationStates
                                            (monitoring clientDependencies)
                                            workspace >>= runNextStepOnErrorOrProposeAvailableWorkspaceCommandAgain
        GotoWorkOnAGoal               _ -> runWorkOnAGoal      currentStep >>= runNextStep
        GotoWorkOnWorkspaces          _ -> runWorkOnWorkspaces currentStep >>= runNextStep
        QuitCommand                   _ -> runQuitCLI                      >>= runNextStep

    workspaceActions :: Workspace -> [WorkspaceCommand]
    workspaceActions Workspace {goalStats = GoalStats {toBeAccomplished = goalsToBeAccomplished}}
      | goalsToBeAccomplished > 0 =
          [ RenameWorkspaceCommand        "Rename" ,
            SetNewGoalCommand             "Set A New Goal",
            ListCommandsReceived          "List Commands Received",
            ListCommandResponsesProduced  "List Command Responses Produced",
            ListEventsGenerated           "List Events Generated",
            ListValidationStates          "List Validation Dependencies History",
            GotoWorkOnAGoal               "Work On A Goal",
            GotoWorkOnWorkspaces          "Work On Another Workspace",
            QuitCommand                   "Quit"]
      | otherwise =
                [ RenameWorkspaceCommand       "Rename" ,
                  SetNewGoalCommand            "Set A New Goal",
                  ListCommandsReceived         "List Commands Received",
                  ListCommandResponsesProduced "List Command Responses Produced",
                  ListEventsGenerated          "List Events Generated",
                  ListValidationStates         "List Validation Dependencies History",
                  GotoWorkOnWorkspaces         "Work On Another Workspace",
                  QuitCommand                  "Quit"]

    stylizeAction :: Workspace -> WorkspaceCommand -> Stylized
    stylizeAction Workspace {goalStats = GoalStats {toBeAccomplished = goalsToBeAccomplished}} workspaceAction
      | goalsToBeAccomplished > 0 = case workspaceAction of
          RenameWorkspaceCommand       description -> fg white <> text description
          SetNewGoalCommand            description -> fg white <> text description <> "\nInfra-Monitoring"
          ListCommandsReceived         description -> fg white <> text description
          ListCommandResponsesProduced description -> fg white <> text description
          ListEventsGenerated          description -> fg white <> text description
          ListValidationStates         description -> fg white <> text description <> "\nNavigation"
          GotoWorkOnAGoal              description -> fg white <> text description
          GotoWorkOnWorkspaces         description -> fg white <> text description
          QuitCommand                  description -> fg white <> text description
      | otherwise  = case workspaceAction of
          RenameWorkspaceCommand       description -> fg white <> text description
          SetNewGoalCommand            description -> fg white <> text description <> "\nInfra-Monitoring"
          ListCommandsReceived         description -> fg white <> text description
          ListCommandResponsesProduced description -> fg white <> text description
          ListEventsGenerated          description -> fg white <> text description
          ListValidationStates         description -> fg white <> text description <> "\nNavigation"
          GotoWorkOnWorkspaces         description -> fg white <> text description
          QuitCommand                  description -> fg white <> text description
          _ -> fg red <> "Not handle"

    runRenameWorkspace :: Step WorkOnAWorkspace -> Byline IO (Either StepError (Step WorkOnAWorkspace))
    runRenameWorkspace currentStep @ (WorkOnAWorkspaceStep
                                        workOnAWorkspace
                                        cliDependencies  @ Dependencies { clientDependencies}
                                        workspace @ Workspace {workspaceId}
                                        workOnWorkspaces) = do
      commandId <- liftIO $ nextRandom
      sayLn $ fg green <> "generating a new Command Id (" <> text (toText commandId) <>") "
      workspaceNewName <- askUntil "Enter a new workspace name : " Nothing atLeastThreeChars
      response <- liftIO $ sendCommandAndWaitTillProcessed
                            (commandSourcer  clientDependencies)
                            RenameWorkspace {commandId ,
                                             workspaceId,
                                             workspaceNewName}
      case response of
        Left  errorDescription -> return $ Left $ StepError {currentStep , errorDescription = show errorDescription}
        Right CommandFailed {reason} ->  do
          sayLn $ fg red <> "> The command failed : "<> (text . pack ) reason
          displayEndOfACommand
          return $ Right currentStep
        Right CommandSuccessfullyProcessed {} ->  do
          sayLn $ fg green <> "> The command has been successfully processed... "
          displayEndOfACommand
          (liftIO $ fetchWorkspace (read  clientDependencies) workspaceId)
           <&> either
                (\error -> Left $ StepError {currentStep ,errorDescription = show error})
                (maybe
                  (Left $ StepError {currentStep ,errorDescription = "Workspace asked does not exist"})
                  (\workspace -> Right $ WorkOnAWorkspaceStep run cliDependencies workspace workOnWorkspaces))

    runSetNewGoal :: Step WorkOnAWorkspace -> Byline IO (Either StepError (Step WorkOnAWorkspace))
    runSetNewGoal currentStep @ (WorkOnAWorkspaceStep
                                    workOnAWorkspace
                                    cliDependencies  @ Dependencies { clientDependencies}
                                    workspace @ Workspace {workspaceId}
                                    workOnWorkspaces) = do
        commandId <- liftIO $ nextRandom
        sayLn $ fg green <> "generating a new Command Id (" <> text (toText commandId) <>") "
        goalId <- liftIO $ nextRandom
        sayLn $ fg green <> "generating a new goal Id (" <> text (toText goalId) <>") "
        goalDescription <- askUntil "Enter a goal description : " Nothing atLeastThreeChars
        response <- liftIO $ sendCommandAndWaitTillProcessed (commandSourcer  clientDependencies) SetGoal {commandId,
                                                                            workspaceId,
                                                                            goalId,
                                                                            goalDescription}
        case response of
          Left  errorDescription -> return $ Left $ StepError {currentStep , errorDescription = show errorDescription}
          Right CommandFailed {reason} ->  do
            sayLn $ fg red <> "> The command failed : "<> (text . pack ) reason
            displayEndOfACommand
            return $ Right currentStep
          Right CommandSuccessfullyProcessed {} ->  do
            sayLn $ fg green <> "> The command has been successfully processed... "
            displayEndOfACommand
            (liftIO $ fetchWorkspace (read  clientDependencies) workspaceId)
             <&> either
                  (\error -> Left $ StepError {currentStep ,errorDescription = show error})
                  (maybe
                    (Left $ StepError {currentStep ,errorDescription = "Workspace asked does not exist"})
                    (\workspace -> Right $ WorkOnAWorkspaceStep run cliDependencies workspace workOnWorkspaces))



    runWorkOnAGoal :: Step WorkOnAWorkspace -> Byline IO (Either StepError (Step WorkOnAGoal))
    runWorkOnAGoal currentStep @ (WorkOnAWorkspaceStep
                                    workOnAWorkspace
                                    cliDependencies  @ Dependencies { clientDependencies}
                                    workspace @ Workspace {workspaceId}
                                    workOnWorkspaces) = do
     displayBeginningOfACommand
     safeResponse <-  liftIO $ fetchGoals (read  clientDependencies) workspaceId
     case safeResponse of
      Left stepError -> return $ Left StepError {currentStep, errorDescription = show stepError }
      Right goals -> do
         sayLn "Goals"
         let menuConfig = renderPrefixAndSuffixForDynamicGsdMenu (menu goals displayGoalForSelection)
             prompt     = "> please choose an action (provide the index) : "
             onError    = "> please enter a valid index..."
         goal <- askWithMenuRepeatedly menuConfig prompt onError
         displayEndOfACommand
         return $ Right $ WorkOnAGoalStep GoalCLI.run cliDependencies workspace goal workOnAWorkspace workOnWorkspaces

    runWorkOnWorkspaces :: Step WorkOnAWorkspace -> Byline IO (Either StepError (Step WorkOnWorkspaces))
    runWorkOnWorkspaces (WorkOnAWorkspaceStep workOnAWorkspace cliDependencies  workspace workOnWorkspaces) = do
      return $ Right $ WorkOnWorkspacesStep workOnWorkspaces cliDependencies


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
           <> fg green <> (text . pack  .show) toBeAccomplished <> " goal(s) \n"
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

    runNextStepOnErrorOrProposeAvailableWorkspaceCommandAgain :: forall stepType. Either StepError (Step stepType) ->
                                                                              Byline IO ()
    runNextStepOnErrorOrProposeAvailableWorkspaceCommandAgain =
      either
        (\error -> runNextStep $ Left error)
        (\step -> proposeAvailableWorkspaceCommands)
