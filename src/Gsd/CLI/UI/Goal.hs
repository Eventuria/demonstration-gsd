{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Gsd.CLI.UI.Goal (run)where

import Data.Functor
import Prelude hiding (length,readClientDependencies)
import Data.Text hiding (foldr,map)
import qualified  Data.List as List
import Data.UUID.V4
import Data.UUID
import Control.Monad.IO.Class (MonadIO(liftIO))
import System.Console.Byline hiding (askWithMenuRepeatedly)
import ByLine.Wrapper
import Gsd.Write.Flow.Sourcer.Client.Client
import Gsd.CLI.UI.Quit (runQuitCLI)
import Gsd.CLI.Dependencies
import Gsd.Read.API.Client.Client (fetchActions,fetchGoal,fetchGoals)
import Gsd.Read.Model.Goal
import Gsd.Read.Model.Action
import Gsd.CLI.Workflow.Steps
import Gsd.Write.Model.Commands.Command
import CQRS.Write.Aggregate.Commands.CommandId
import Gsd.Write.Model.Core
import CQRS.Write.Aggregate.Commands.Responses.CommandResponse
import Gsd.CLI.UI.Greetings
import Gsd.Read.Model.Workspace
import Gsd.Read.Model.ActionStats
import Gsd.Read.Model.GoalStats
import qualified Gsd.CLI.UI.Monitoring as MonitoringCLI
import Gsd.CLI.UI.Monitoring (runMonitoringCommand)

data GoalCommands = -- Goal Commands
                    RefineGoalDescriptionCommand  Text
                  | ChangeGoalStatus              Text
                    -- Action Commands
                  | ActionizeOnGoalCommand        Text
                  | NotifyActionCompletedCommand  Text
                    -- Infra-Monitoring
                  | ListCommandsReceived         Text
                  | ListCommandResponsesProduced Text
                  | ListEventsGenerated          Text
                  | ListValidationStates         Text
                    -- Navigation
                  | GotoWorkOnAGoal             Text
                  | GotoWorkOnWorkspace         Text
                  | GotoWorkOnWorkspaces        Text
                  | QuitCommand                 Text

run :: WorkOnAGoalStepHandle
run cliDependencies   @ Dependencies {writeClientDependencies,monitoringClientDependencies,readClientDependencies}
           workspace @ Workspace {workspaceId}
           goal @ Goal {goalId}
           workOnWorkspace
           workOnWorkspaces = do
  let currentStep = WorkOnAGoalStep run cliDependencies workspace goal workOnWorkspace workOnWorkspaces
  safeResponse  <- liftIO $ fetchActions readClientDependencies workspaceId goalId
  case safeResponse of
    Left error -> runNextStep $ Left StepError {currentStep, errorDescription = show error }
    Right actions -> do
      sayLn $ displayGoal workspace goal actions
      proposeAvailableGoalCommands

  where
    proposeAvailableGoalCommands :: Byline IO ()
    proposeAvailableGoalCommands = do
      sayLn "Goal Commands"
      let menuConfig = renderPrefixAndSuffixForDynamicGsdMenu $
                          menu (goalActions workspace goal) (stylizeAction goal)
          prompt     = "please choose a command (provide the index) : "
          onError    = "please enter a valid index..."
          currentStep = WorkOnAGoalStep run cliDependencies workspace goal workOnWorkspace workOnWorkspaces

      answer <- askWithMenuRepeatedly menuConfig prompt onError
      case answer of
        RefineGoalDescriptionCommand _ -> runRefineGoalDescriptionCommand currentStep >>= runNextStep
        ChangeGoalStatus             _ -> runChangeGoalStatus             currentStep >>= runNextStep
        ActionizeOnGoalCommand       _ -> runActionizeOnGoalCommand       currentStep >>= runNextStep
        NotifyActionCompletedCommand _ -> runNotifyActionCompletedCommand currentStep >>= runNextStep
        ListCommandsReceived         _ -> runMonitoringCommand            currentStep
                                              MonitoringCLI.ListCommandsReceived
                                              monitoringClientDependencies
                                              workspace >>= runNextStepOnErrorOrProposeAvailableGoalCommandsAgain
        ListCommandResponsesProduced _ -> runMonitoringCommand            currentStep
                                              MonitoringCLI.ListCommandResponsesProduced
                                              monitoringClientDependencies
                                              workspace >>= runNextStepOnErrorOrProposeAvailableGoalCommandsAgain
        ListEventsGenerated          _ -> runMonitoringCommand            currentStep
                                              MonitoringCLI.ListEventsGenerated
                                              monitoringClientDependencies
                                              workspace >>= runNextStepOnErrorOrProposeAvailableGoalCommandsAgain
        ListValidationStates         _ -> runMonitoringCommand            currentStep
                                              MonitoringCLI.ListValidationStates
                                              monitoringClientDependencies
                                              workspace >>= runNextStepOnErrorOrProposeAvailableGoalCommandsAgain
        GotoWorkOnAGoal               _ -> runWorkOnAGoal                 currentStep >>= runNextStep
        GotoWorkOnWorkspace           _ -> runWorkOnWorkspace             currentStep >>= runNextStep
        GotoWorkOnWorkspaces          _ -> runWorkOnWorkspaces            currentStep >>= runNextStep
        QuitCommand                   _ -> runQuitCLI                                 >>= runNextStep

    goalActions :: Workspace -> Goal ->  [GoalCommands]
    goalActions Workspace {goalStats = GoalStats {total = totalGoals}}
                Goal {status, actionStats = ActionStats {opened }} =
        (case status of
          status | status == Accomplished || status == GivenUp ->
            [  RefineGoalDescriptionCommand    "Refine The Goal Description"]
          _ ->
            [  RefineGoalDescriptionCommand    "Refine The Goal Description" ,
               ChangeGoalStatus                "Change The Status",
               ActionizeOnGoalCommand          "Define A New Action"]
             ++ (case opened of
                    opened | opened > 0   ->
                      [NotifyActionCompletedCommand    "Notify An Action Completed"]
                    _ -> []))
     ++ [      ListCommandsReceived            "List Commands Received",
               ListCommandResponsesProduced    "List Command Responses Produced",
               ListEventsGenerated             "List Events Generated",
               ListValidationStates            "List Validation Dependencies History"]
     ++ (case totalGoals of
            totalGoals | totalGoals > 0 ->
              [GotoWorkOnAGoal                 "Work On Another Goal"]
            _ -> [])
     ++ [      GotoWorkOnWorkspace             "Work On The Workspace ",
               GotoWorkOnWorkspaces            "Work On Another Workspace",
               QuitCommand                            "Quit"]

    stylizeAction :: Goal -> GoalCommands -> Stylized 
    stylizeAction Goal {status, actionStats = ActionStats {opened }}
                  goalCommands
        | status == Accomplished || status == GivenUp = case goalCommands of
            RefineGoalDescriptionCommand description -> fg white <> text description <> "\nInfra-Monitoring"
            ListCommandsReceived         description -> fg white <> text description
            ListCommandResponsesProduced description -> fg white <> text description
            ListEventsGenerated          description -> fg white <> text description
            ListValidationStates         description -> fg white <> text description <> "\nNavigation"
            GotoWorkOnAGoal              description -> fg white <> text description
            GotoWorkOnWorkspace          description -> fg white <> text description
            GotoWorkOnWorkspaces         description -> fg white <> text description
            QuitCommand                  description -> fg white <> text description
            _ -> fg red <> "Not handle"
        | opened == 0 = case goalCommands of
            RefineGoalDescriptionCommand description -> fg white <> text description
            ChangeGoalStatus             description -> fg white <> text description <> "\nAction Commands"
            ActionizeOnGoalCommand       description -> fg white <> text description <> "\nInfra-Monitoring"
            ListCommandsReceived         description -> fg white <> text description
            ListCommandResponsesProduced description -> fg white <> text description
            ListEventsGenerated          description -> fg white <> text description
            ListValidationStates         description -> fg white <> text description <> "\nNavigation"
            GotoWorkOnAGoal              description -> fg white <> text description
            GotoWorkOnWorkspace          description -> fg white <> text description
            GotoWorkOnWorkspaces         description -> fg white <> text description
            QuitCommand                  description -> fg white <> text description
            _ -> fg red <> "Not handle"
        | otherwise = case goalCommands of
            RefineGoalDescriptionCommand description -> fg white <> text description
            ChangeGoalStatus             description -> fg white <> text description <> "\nAction Commands"
            ActionizeOnGoalCommand       description -> fg white <> text description
            NotifyActionCompletedCommand description -> fg white <> text description <> "\nInfra-Monitoring"
            ListCommandsReceived         description -> fg white <> text description
            ListCommandResponsesProduced description -> fg white <> text description
            ListEventsGenerated          description -> fg white <> text description
            ListValidationStates         description -> fg white <> text description <> "\nNavigation"
            GotoWorkOnAGoal              description -> fg white <> text description
            GotoWorkOnWorkspace          description -> fg white <> text description
            GotoWorkOnWorkspaces         description -> fg white <> text description
            QuitCommand                  description -> fg white <> text description


    runWorkOnWorkspaces :: Step WorkOnAGoal -> Byline IO (Either StepError (Step WorkOnWorkspaces))
    runWorkOnWorkspaces  (WorkOnAGoalStep _ cliDependencies _ _ _ workOnWorkspaces) = do
      return $ Right $ WorkOnWorkspacesStep workOnWorkspaces cliDependencies


    runNotifyActionCompletedCommand :: Step WorkOnAGoal -> Byline IO (Either StepError (Step WorkOnAGoal))
    runNotifyActionCompletedCommand currentStep @ (WorkOnAGoalStep
                                                       workOnAGoalStepHandle
                                                       cliDependencies @ Dependencies {readClientDependencies, writeClientDependencies}
                                                       workspace @ Workspace {workspaceId}
                                                       goal @ Goal {goalId}
                                                       workOnWorkspace
                                                       workOnWorkspaces) = do
      safeResponse <-  liftIO $ fetchActions readClientDependencies workspaceId goalId
      case safeResponse of
        Left error -> return $ Left StepError {currentStep, errorDescription = show error }
        Right actions -> do
           let menuConfig = banner "Available actions :" $ menu actions stylizeAction
               prompt     = "please choose the action you consider Accomplished (provide the index) : "
               onError    = "please enter a valid index..."
           Action {actionId} <- askWithMenuRepeatedly menuConfig prompt onError
           commandId <- liftIO $ nextRandom
           sayLn $ fg green <> "generated a new Command Id (" <> text (toText commandId) <>") "
           response <- liftIO $ sendCommandAndWaitTillProcessed writeClientDependencies NotifyActionCompleted {commandId ,
                                                                                             workspaceId ,
                                                                                             goalId,
                                                                                             actionId}
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
        where
          stylizeAction :: Action -> Stylized
          stylizeAction Action {indexation,details} =
            fg cyan <> "Action (" <> (text $ (pack.show) indexation) <> " , " <> text details <> " )"



    runWorkOnWorkspace :: Step WorkOnAGoal -> Byline IO (Either StepError (Step WorkOnAWorkspace))
    runWorkOnWorkspace (WorkOnAGoalStep _ cliDependencies workspace goal workOnWorkspace workOnWorkspaces) = do
      return $ Right $ WorkOnAWorkspaceStep workOnWorkspace  cliDependencies workspace workOnWorkspaces

    runRefineGoalDescriptionCommand :: Step WorkOnAGoal -> Byline IO (Either StepError (Step WorkOnAGoal))
    runRefineGoalDescriptionCommand currentStep @ (WorkOnAGoalStep
                                                     workOnAGoalStepHandle
                                                     cliDependencies @ Dependencies {readClientDependencies, writeClientDependencies}
                                                     workspace @ Workspace {workspaceId}
                                                     goal @ Goal {goalId}
                                                     workOnWorkspace
                                                     workOnWorkspaces) = do
      commandId <- liftIO $ nextRandom
      sayLn $ fg green <> "generated a new Command Id (" <> text (toText commandId) <>") "
      refinedGoalDescription <- askUntil "Enter a new goal description : " Nothing atLeastThreeChars
      response <- liftIO $ sendCommandAndWaitTillProcessed writeClientDependencies RefineGoalDescription {commandId ,
                                                                      workspaceId ,
                                                                      goalId,
                                                                      refinedGoalDescription}
      case response of
          Left  errorDescription -> return $ Left $ StepError {currentStep , errorDescription = show errorDescription}
          Right CommandFailed {reason} ->  do
            sayLn $ fg red <> "> The command failed : "<> (text . pack ) reason
            displayEndOfACommand
            return $ Right currentStep
          Right CommandSuccessfullyProcessed {} ->  do
            sayLn $ fg green <> "> The command has been successfully processed... "
            displayEndOfACommand
            (liftIO $ (fetchGoal readClientDependencies workspaceId goalId))
             <&> either
                  (\error -> Left $ StepError {currentStep ,errorDescription = show error})
                  (maybe
                    (Left $ StepError {currentStep ,errorDescription = "Goal asked does not exist"})
                    (\goal -> Right $ WorkOnAGoalStep
                                          run
                                          cliDependencies
                                          workspace
                                          goal
                                          workOnWorkspace
                                          workOnWorkspaces))





    runActionizeOnGoalCommand :: Step WorkOnAGoal -> Byline IO (Either StepError (Step WorkOnAGoal))
    runActionizeOnGoalCommand currentStep @ (WorkOnAGoalStep
                                                workOnAGoalStepHandle
                                                cliDependencies @ Dependencies {readClientDependencies, writeClientDependencies}
                                                workspace @ Workspace {workspaceId}
                                                goal @ Goal {goalId}
                                                workOnWorkspace
                                                workOnWorkspaces) = do
      commandId <- liftIO $ nextRandom
      sayLn $ fg green <> "generated a new Command Id (" <> text (toText commandId) <>") "
      actionId <- liftIO $ nextRandom
      sayLn $ fg green <> "generated a new Action Id (" <> text (toText commandId) <>") "
      actionDetails <- askUntil "Enter the details of the action : " Nothing atLeastThreeChars
      response <- liftIO $ sendCommandAndWaitTillProcessed writeClientDependencies ActionizeOnGoal {commandId ,
                                                                                  workspaceId ,
                                                                                  goalId,
                                                                                  actionId ,
                                                                                  actionDetails}
      case response of
          Left  errorDescription -> return $ Left $ StepError {currentStep , errorDescription = show errorDescription}
          Right CommandFailed {reason} ->  do
            sayLn $ fg red <> "> The command failed : "<> (text . pack ) reason
            displayEndOfACommand
            return $ Right currentStep
          Right CommandSuccessfullyProcessed {} ->  do
            sayLn $ fg green <> "> The command has been successfully processed... "
            displayEndOfACommand
            (liftIO $ (fetchGoal readClientDependencies workspaceId goalId))
             <&> either
                  (\error -> Left $ StepError {currentStep ,errorDescription = show error})
                  (maybe
                    (Left $ StepError {currentStep ,errorDescription = "Goal asked does not exist"})
                    (\goal -> Right $ WorkOnAGoalStep
                                          run
                                          cliDependencies
                                          workspace
                                          goal
                                          workOnWorkspace
                                          workOnWorkspaces))


    runWorkOnAGoal :: Step WorkOnAGoal -> Byline IO (Either StepError (Step WorkOnAGoal))
    runWorkOnAGoal currentStep @ (WorkOnAGoalStep
                                              workOnAGoalStepHandle
                                              cliDependencies @ Dependencies {readClientDependencies, writeClientDependencies}
                                              workspace @ Workspace {workspaceId}
                                              goal @ Goal {goalId}
                                              workOnWorkspace
                                              workOnWorkspaces) = do
     displayBeginningOfACommand
     safeResponse <-  liftIO $ fetchGoals readClientDependencies workspaceId
     case safeResponse of
      Left error -> return $ Left StepError {currentStep, errorDescription = show error }
      Right goals -> do
         sayLn "Goals"
         let menuConfig = renderPrefixAndSuffixForDynamicGsdMenu (menu goals displayGoalForSelection)
             prompt     = "> please choose an action (provide the index) : "
             onError    = "> please enter a valid index..."
         goalSelected <- askWithMenuRepeatedly menuConfig prompt onError
         displayEndOfACommand
         return $ Right $ WorkOnAGoalStep run cliDependencies workspace goalSelected workOnWorkspace workOnWorkspaces


    runChangeGoalStatus :: Step WorkOnAGoal -> Byline IO (Either StepError (Step WorkOnAGoal))
    runChangeGoalStatus currentStep @ (WorkOnAGoalStep
                                          workOnAGoalStepHandle
                                          cliDependencies @ Dependencies {readClientDependencies, writeClientDependencies}
                                          workspace @ Workspace {workspaceId}
                                          goal @ Goal {goalId}
                                          workOnWorkspace
                                          workOnWorkspaces) = do
      safeResponse <- liftIO $ (fetchGoal readClientDependencies workspaceId goalId)
      case safeResponse of
        Left applicationError  -> return $ Left $ StepError {
                                                    currentStep ,
                                                    errorDescription = show applicationError}
        Right (Nothing)  -> return $ Left $ StepError {
                                              currentStep ,
                                              errorDescription = "Goal asked does not exist"}
        Right (Just goal @ Goal {status}) -> do
          case (getNextStatusAvailable status) of
             [] -> do
                sayLn $ fg green <> "No status avalaible (final state)"
                return $ Right currentStep
             getNextStatusAvailable -> do
                sayLn "Available status on the selected goal : "
                let menuConfig = renderPrefixAndSuffixForDynamicGsdMenu
                                    $ menu getNextStatusAvailable (\status -> fg green <> (text.pack.show) status)
                    prompt     = "please choose an action (provide the index) : "
                    onError    = "please enter a valid index..."

                nextStatus <- askWithMenuRepeatedly menuConfig prompt onError
                commandId <- liftIO $ nextRandom
                sayLn $ fg green <> "generated a new Command Id (" <> text (toText commandId) <>") "
                commandToSent <- getCommandToSend nextStatus commandId goalId workspaceId
                response <- liftIO $ sendCommandAndWaitTillProcessed writeClientDependencies commandToSent
                case response of
                  Left  errorDescription -> return $ Left $ StepError {currentStep , errorDescription = show errorDescription}
                  Right CommandFailed {reason} ->  do
                    sayLn $ fg red <> "> The command failed : "<> (text . pack ) reason
                    displayEndOfACommand
                    return $ Right currentStep
                  Right CommandSuccessfullyProcessed {} ->  do
                    sayLn $ fg green <> "> The command has been successfully processed... "
                    displayEndOfACommand
                    (liftIO $ (fetchGoal readClientDependencies workspaceId goalId))
                     <&> either
                          (\error -> Left $ StepError {currentStep ,errorDescription = show error})
                          (maybe
                            (Left $ StepError {currentStep ,errorDescription = "Goal asked does not exist"})
                            (\goal -> Right $ WorkOnAGoalStep
                                                  run
                                                  cliDependencies
                                                  workspace
                                                  goal
                                                  workOnWorkspace
                                                  workOnWorkspaces))


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


    displayGoal :: Workspace -> Goal -> [Action] ->  Stylized
    displayGoal workspace @ Workspace {workspaceName}
                goal @ Goal {description}
                actions =
          fg green <> "Workspaces"
       <> fg white <>" / "
       <> fg cyan <> "\""
       <> fg cyan <> text workspaceName
       <> fg cyan <> "\""
       <> fg white <>" / "
       <> fg green <> "Goals"
       <> fg white <>" / "
       <> fg cyan <> "\""
       <> fg cyan <> text description
       <> fg cyan <> "\"\n"
       <> displayGoalSummary goal
       <> displayActions displayActionForGoalSummary actions
       <> fg white <> "------------------------------------------"


    displayGoalSummary :: Goal -> Stylized
    displayGoalSummary Goal { status, actionStats = ActionStats {total,
                                                         completed,
                                                         opened}}
       | total == 0 = fg white <> "Summary : \n"
                   <> fg white <> " - Status : " <> fg green  <> (text . pack  .show) status <> "\n"
                   <> fg white <> " - " <> fg green  <>"(No Actions added so far...)\n"
       | otherwise =
              fg white <>"Summary\n"
           <> fg white <>" - Status : "<> fg green <> (text . pack  .show) status <> "\n"
           <> fg white <>" - Actions : "<> fg green <> (text . pack  .show) total <> "\n"
             <> fg white <>"   - Todo : " <> fg green  <> (text . pack  .show) opened <> " action(s)\n"
             <> fg white <>"   - Done : " <> fg green  <> (text . pack  .show) completed <> " action(s)\n"

    displayActions :: (Action -> Stylized) -> [Action] ->  Stylized
    displayActions displayAction actions
        | List.length actions == 0 = ""
        | otherwise =
               fg white <>"Actions\n"
            <> (foldr (<>) "" (map (\action -> displayAction action) actions))

    displayActionForGoalSummary :: Action -> Stylized
    displayActionForGoalSummary Action { indexation, details , status} =
          fg white <> "  " <> (text . pack . show) indexation <> "- " <> fg green <> text details
       <> fg green <> " (" <> (text . pack . show) status <> ")\n"

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


    runNextStepOnErrorOrProposeAvailableGoalCommandsAgain :: forall stepType. Either StepError (Step stepType) ->
                                                                              Byline IO ()
    runNextStepOnErrorOrProposeAvailableGoalCommandsAgain =
      either
        (\error -> runNextStep $ Left error)
        (\step -> proposeAvailableGoalCommands)
