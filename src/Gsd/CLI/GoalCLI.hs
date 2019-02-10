{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Gsd.CLI.GoalCLI (run)where

import Prelude hiding (length)
import Data.Function ((&))
import qualified Servant.Client.Streaming as S
import Data.Text hiding (foldr,map)
import qualified  Data.List as List
import Data.UUID.V4
import Data.UUID
import Control.Monad.IO.Class (MonadIO(liftIO))
import System.Console.Byline hiding (askWithMenuRepeatedly)
import Gsd.CLI.ByLineWrapper
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Gsd.Write.Client
import Gsd.CLI.QuitCLI (runQuitCLI)
import Servant.Client
import Gsd.Clients
import Gsd.Read.Client (streamAction,fetchGoal,streamGoal)
import Gsd.Read.Goal
import Gsd.Read.Action
import Gsd.CLI.Steps
import Gsd.Write.Commands.Command
import Cqrs.Write.Aggregate.Commands.CommandId
import Gsd.Write.Core
import Cqrs.Write.Aggregate.Commands.Responses.CommandResponse
import Gsd.CLI.Greetings
import Gsd.Read.Workspace
import Gsd.Read.ActionStats
import Gsd.Read.GoalStats
import qualified Streamly.Safe as StreamlySafe
import Gsd.CLI.WorkspaceMonitoringCLI (runListCommandReceived,
                                           runListCommandResponseReceived,
                                           runListEventsGenerated,
                                           runListValidationStateHistory)
import Control.Exception


data GoalCommands = -- Goal Commands
                    RefineGoalDescriptionCommand Text
                  | ChangeGoalStatus              Text
                    -- Action Commands
                  | ActionizeOnGoalCommand        Text
                  | NotifyActionCompletedCommand  Text
                    -- Infra-Monitoring
                  | ListCommandsReceived        Text
                  | ListCommandResponseProduced Text
                  | ListEventsGenerated         Text
                  | ListValidationStateHistory  Text
                    -- Navigation
                  | GotoWorkOnAGoal             Text
                  | GotoWorkOnWorkspace         Text
                  | GotoWorkOnWorkspaces        Text
                  | Quit                        Text




run :: WorkOnAGoalStepHandle
run clients   @ Clients {writeApiUrl,gsdMonitoringApiUrl,gsdReadApiUrl}
           workspace @ Workspace {workspaceId}
           goal @ Goal {goalId}
           workOnWorkspace
           workOnWorkspaces = do
  let currentStep = WorkOnAGoalStep run clients workspace goal workOnWorkspace workOnWorkspaces
  manager <- liftIO $ newManager defaultManagerSettings
  result  <- liftIO $ S.withClientM
                          (streamAction workspaceId goalId)
                          (S.mkClientEnv manager gsdReadApiUrl)
                          $ (\result -> case result of
                                     Left error -> return $ Left $ toException $ error
                                     Right stream -> do
                                        safeResponse <- stream & StreamlySafe.toList
                                        return safeResponse)


  case result of
    Left error -> runNextStep $ Left StepError {currentStep, errorDescription = show error }
    Right actions -> do
      sayLn $ displayGoal workspace goal actions
      askNextStep currentStep

  where

    askNextStep :: Step WorkOnAGoal -> Byline IO ()
    askNextStep currentStep = do
      sayLn "Goal Commands"
      let menuConfig = renderPrefixAndSuffixForDynamicGsdMenu $
                          menu (goalActions workspace goal) (stylizeAction goal)
          prompt     = "please choose a command (provide the index) : "
          onError    = "please enter a valid index..."


      answer <- askWithMenuRepeatedly menuConfig prompt onError
      case answer of
        RefineGoalDescriptionCommand description ->
          runRefineGoalDescriptionCommand currentStep >>= runNextStep
        ChangeGoalStatus description ->
          runChangeGoalStatus currentStep >>= runNextStep
        ActionizeOnGoalCommand description ->
          runActionizeOnGoalCommand currentStep >>= runNextStep
        NotifyActionCompletedCommand description ->
          runNotifyActionCompletedCommand currentStep >>= runNextStep
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
        GotoWorkOnWorkspace description ->
          runWorkOnWorkspace description currentStep >>= runNextStep
        GotoWorkOnWorkspaces description ->
          runWorkOnWorkspaces description currentStep >>= runNextStep
        Quit description -> runQuitCLI

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
               ListCommandResponseProduced     "List Command Responses Produced",
               ListEventsGenerated             "List Events Generated",
               ListValidationStateHistory      "List Validation State History"]
     ++ (case totalGoals of
            totalGoals | totalGoals > 0 ->
              [GotoWorkOnAGoal                 "Work On Another Goal"]
            _ -> [])
     ++ [      GotoWorkOnWorkspace             "Work On The Workspace ",
               GotoWorkOnWorkspaces            "Work On Another Workspace",
               Quit                            "Quit"]

    stylizeAction :: Goal -> GoalCommands -> Stylized 
    stylizeAction Goal {status, actionStats = ActionStats {opened }}
                  goalCommands
        | status == Accomplished || status == GivenUp = case goalCommands of
            RefineGoalDescriptionCommand description -> fg white <> text description <> "\nInfra-Monitoring"
            ListCommandsReceived description ->  fg white  <>  text description
            ListCommandResponseProduced description -> fg white <> text description
            ListEventsGenerated description -> fg white <> text description
            ListValidationStateHistory description -> fg white  <> text description <> "\nNavigation"
            GotoWorkOnAGoal description -> fg white <> text description
            GotoWorkOnWorkspace description -> fg white <> text description
            GotoWorkOnWorkspaces description -> fg white <> text description
            Quit description -> fg white <> text description
            _ -> fg red <> "Not handle"
        | opened == 0 = case goalCommands of
            RefineGoalDescriptionCommand description -> fg white <> text description
            ChangeGoalStatus description -> fg white <> text description <> "\nAction Commands"
            ActionizeOnGoalCommand description -> fg white <> text description <> "\nInfra-Monitoring"
            ListCommandsReceived description ->  fg white  <>  text description
            ListCommandResponseProduced description -> fg white <> text description
            ListEventsGenerated description -> fg white <> text description
            ListValidationStateHistory description -> fg white  <> text description <> "\nNavigation"
            GotoWorkOnAGoal description -> fg white <> text description
            GotoWorkOnWorkspace description -> fg white <> text description
            GotoWorkOnWorkspaces description -> fg white <> text description
            Quit description -> fg white <> text description
            _ -> fg red <> "Not handle"
        | otherwise = case goalCommands of
            RefineGoalDescriptionCommand description -> fg white <> text description
            ChangeGoalStatus description -> fg white <> text description <> "\nAction Commands"
            ActionizeOnGoalCommand description -> fg white <> text description
            NotifyActionCompletedCommand description -> fg white <> text description <> "\nInfra-Monitoring"
            ListCommandsReceived description ->  fg white  <>  text description
            ListCommandResponseProduced description -> fg white <> text description
            ListEventsGenerated description -> fg white <> text description
            ListValidationStateHistory description -> fg white  <> text description <> "\nNavigation"
            GotoWorkOnAGoal description -> fg white <> text description
            GotoWorkOnWorkspace description -> fg white <> text description
            GotoWorkOnWorkspaces description -> fg white <> text description
            Quit description -> fg white <> text description


    runWorkOnWorkspaces :: Text -> Step WorkOnAGoal -> Byline IO (Either StepError (Step WorkOnWorkspaces))
    runWorkOnWorkspaces description currentStep @ (WorkOnAGoalStep
                                                      workOnAGoalStepHandle
                                                      clients
                                                      workspaceId
                                                      goal @ Goal {goalId}
                                                      workOnWorkspace
                                                      workOnWorkspaces) = do
      sayLn $ fg green <> (text . pack .show) description <> "selected "
      return $ Right $ WorkOnWorkspacesStep workOnWorkspaces clients


    runNotifyActionCompletedCommand :: Step WorkOnAGoal -> Byline IO (Either StepError (Step WorkOnAGoal))
    runNotifyActionCompletedCommand currentStep @ (WorkOnAGoalStep
                                                       workOnAGoalStepHandle
                                                       clients @ Clients {gsdReadApiUrl,writeApiUrl}
                                                       workspace @ Workspace {workspaceId}
                                                       goal @ Goal {goalId}
                                                       workOnWorkspace
                                                       workOnWorkspaces) = do
      manager <- liftIO $ newManager defaultManagerSettings
      result <-  liftIO $ S.withClientM
                            (streamAction workspaceId goalId)
                            (S.mkClientEnv manager gsdReadApiUrl)
                            $ (\result -> case result of
                                             Left error -> return $ Left $ toException $ error
                                             Right stream -> do
                                                safeResponse <- stream & StreamlySafe.toList
                                                return safeResponse)
      case result of
        Left error -> return $ Left StepError {currentStep, errorDescription = show error }
        Right actions -> do
           let menuConfig = banner "Available actions :" $ menu actions stylizeAction
               prompt     = "please choose the action you consider Accomplished (provide the index) : "
               onError    = "please enter a valid index..."
           Action {actionId} <- askWithMenuRepeatedly menuConfig prompt onError
           commandId <- liftIO $ nextRandom
           sayLn $ fg green <> "generated a new Command Id (" <> text (toText commandId) <>") "
           queryResult <- liftIO $ runClientM
                                      (sendCommandAndWaitResponse
                                          NotifyActionCompleted {commandId , workspaceId , goalId, actionId})
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
                  return $ Right currentStep
        where
          stylizeAction :: Action -> Stylized
          stylizeAction Action {indexation,details} =
            fg cyan <> "Action (" <> (text $ (pack.show) indexation) <> " , " <> text details <> " )"



    runWorkOnWorkspace :: Text -> Step WorkOnAGoal -> Byline IO (Either StepError (Step WorkOnAWorkspace))
    runWorkOnWorkspace description currentStep @ (WorkOnAGoalStep
                                                    workOnAGoalStepHandle
                                                    clients
                                                    workspace @ Workspace {workspaceId}
                                                    goal @ Goal {goalId}
                                                    workOnWorkspace
                                                    workOnWorkspaces) = do
      sayLn $ fg green <> (text . pack .show) description <> "selected "
      return $ Right $ WorkOnAWorkspaceStep workOnWorkspace  clients workspace workOnWorkspaces

    runRefineGoalDescriptionCommand :: Step WorkOnAGoal -> Byline IO (Either StepError (Step WorkOnAGoal))
    runRefineGoalDescriptionCommand currentStep @ (WorkOnAGoalStep
                                                     workOnAGoalStepHandle
                                                     clients @ Clients {writeApiUrl}
                                                     workspace @ Workspace {workspaceId}
                                                     goal @ Goal {goalId}
                                                     workOnWorkspace
                                                     workOnWorkspaces) = do
      commandId <- liftIO $ nextRandom
      sayLn $ fg green <> "generated a new Command Id (" <> text (toText commandId) <>") "
      refinedGoalDescription <- askUntil "Enter a new goal description : " Nothing atLeastThreeChars
      manager  <- liftIO $ newManager defaultManagerSettings
      queryResult <- liftIO $ runClientM
                                (sendCommandAndWaitResponse
                                    RefineGoalDescription {commandId , workspaceId , goalId, refinedGoalDescription})
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
                        (fetchGoal workspaceId goalId)
                        (S.mkClientEnv manager gsdReadApiUrl)
                        $ (\result -> case result of
                          Left servantError -> return $ Left $ StepError {
                                                                currentStep,
                                                                errorDescription = show servantError }
                          Right (Left applicationError)  -> return $ Left $ StepError {
                                                                      currentStep ,
                                                                      errorDescription = show applicationError}
                          Right (Right Nothing)  -> return $ Left $ StepError {
                                                                currentStep ,
                                                                errorDescription = "Goal asked does not exist"}
                          Right (Right (Just goal)) ->
                              return $ Right $ WorkOnAGoalStep
                                                run
                                                clients
                                                workspace
                                                goal
                                                workOnWorkspace
                                                workOnWorkspaces)



    runActionizeOnGoalCommand :: Step WorkOnAGoal -> Byline IO (Either StepError (Step WorkOnAGoal))
    runActionizeOnGoalCommand currentStep @ (WorkOnAGoalStep
                                                workOnAGoalStepHandle
                                                clients @ Clients {writeApiUrl}
                                                workspace @ Workspace {workspaceId}
                                                goal @ Goal {goalId}
                                                workOnWorkspace
                                                workOnWorkspaces) = do
      commandId <- liftIO $ nextRandom
      sayLn $ fg green <> "generated a new Command Id (" <> text (toText commandId) <>") "
      actionId <- liftIO $ nextRandom
      sayLn $ fg green <> "generated a new Action Id (" <> text (toText commandId) <>") "
      actionDetails <- askUntil "Enter the details of the action : " Nothing atLeastThreeChars
      manager  <- liftIO $ newManager defaultManagerSettings
      queryResult <- liftIO $ runClientM
                                (sendCommandAndWaitResponse
                                    ActionizeOnGoal {commandId , workspaceId , goalId, actionId , actionDetails})
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
                        (fetchGoal workspaceId goalId)
                        (S.mkClientEnv manager gsdReadApiUrl)
                        $ (\result -> case result of
                          Left servantError -> return $ Left $ StepError {
                                                                currentStep,
                                                                errorDescription = show servantError }
                          Right (Left applicationError)  -> return $ Left $ StepError {
                                                                      currentStep ,
                                                                      errorDescription = show applicationError}
                          Right (Right Nothing)  -> return $ Left $ StepError {
                                                                currentStep ,
                                                                errorDescription = "Goal asked does not exist"}
                          Right (Right (Just goal)) ->
                              return $ Right $ WorkOnAGoalStep
                                                run
                                                clients
                                                workspace
                                                goal
                                                workOnWorkspace
                                                workOnWorkspaces)


    runWorkOnAGoal :: Step WorkOnAGoal -> Byline IO (Either StepError (Step WorkOnAGoal))
    runWorkOnAGoal currentStep @ (WorkOnAGoalStep
                                              workOnAGoalStepHandle
                                              clients @ Clients {writeApiUrl,gsdReadApiUrl}
                                              workspace @ Workspace {workspaceId}
                                              goal @ Goal {goalId}
                                              workOnWorkspace
                                              workOnWorkspaces) = do
     displayBeginningOfACommand
     manager <- liftIO $ newManager defaultManagerSettings
     result <-  liftIO $ S.withClientM (streamGoal workspaceId)
                             (S.mkClientEnv manager gsdReadApiUrl)
                           $ (\result -> case result of
                                Left error -> return $ Left $ toException $ error
                                Right stream -> do
                                   safeResponse <- stream & StreamlySafe.toList
                                   return safeResponse)
     case result of
      Left error -> return $ Left StepError {currentStep, errorDescription = show error }
      Right goals -> do
         sayLn "Goals"
         let menuConfig = renderPrefixAndSuffixForDynamicGsdMenu (menu goals displayGoalForSelection)
             prompt     = "> please choose an action (provide the index) : "
             onError    = "> please enter a valid index..."
         goalSelected <- askWithMenuRepeatedly menuConfig prompt onError
         displayEndOfACommand
         return $ Right $ WorkOnAGoalStep run clients workspace goalSelected workOnWorkspace workOnWorkspaces


    runChangeGoalStatus :: Step WorkOnAGoal -> Byline IO (Either StepError (Step WorkOnAGoal))
    runChangeGoalStatus currentStep @ (WorkOnAGoalStep
                                          workOnAGoalStepHandle
                                          clients @ Clients {writeApiUrl,gsdReadApiUrl}
                                          workspace @ Workspace {workspaceId}
                                          goal @ Goal {goalId}
                                          workOnWorkspace
                                          workOnWorkspaces) = do
      manager <- liftIO $ newManager defaultManagerSettings
      queryResult <- liftIO $ S.withClientM
                                (fetchGoal workspaceId goalId)
                                (S.mkClientEnv manager gsdReadApiUrl)
                                $ (\e -> return e)
      case queryResult of
        Left servantError -> return $ Left $ StepError {
                                                  currentStep,
                                                  errorDescription = show servantError }
        Right (Left applicationError)  -> return $ Left $ StepError {
                                                              currentStep ,
                                                              errorDescription = show applicationError}
        Right (Right Nothing)  -> return $ Left $ StepError {
                                                      currentStep ,
                                                      errorDescription = "Goal asked does not exist"}
        Right (Right (Just goal @ Goal {status})) -> do
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

                  manager <- liftIO $ newManager defaultManagerSettings
                  queryResult <- liftIO $ runClientM
                                            (sendCommandAndWaitResponse commandToSent)
                                            (mkClientEnv manager writeApiUrl)
                  case queryResult of
                      Left  errorDescription -> return $ Left $ StepError {
                                                                  currentStep ,
                                                                  errorDescription = show errorDescription}
                      Right RequestFailed {reason} ->  do
                        sayLn $ fg red <> "The command has not been sent and taken into account : "
                                       <> (text . pack ) reason
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
                        liftIO $
                            S.withClientM
                                (fetchGoal workspaceId goalId)
                                (S.mkClientEnv manager gsdReadApiUrl)
                                $ \e -> case e of
                                  Left servantError -> return $ Left $ StepError {
                                                                        currentStep,
                                                                        errorDescription = show servantError }
                                  Right (Left applicationError)  -> return $ Left $ StepError {
                                                                              currentStep ,
                                                                              errorDescription = show applicationError}
                                  Right (Right Nothing)  -> return $ Left $ StepError {
                                                                        currentStep ,
                                                                        errorDescription = "Goal asked does not exist"}
                                  Right (Right (Just goal)) ->
                                      return $ Right $ WorkOnAGoalStep
                                                        run
                                                        clients
                                                        workspace
                                                        goal
                                                        workOnWorkspace
                                                        workOnWorkspaces

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


