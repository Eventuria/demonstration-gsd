{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
module Eventuria.GSD.CLI.UI.Monitoring where

import System.Console.Byline
import Control.Monad.IO.Class (MonadIO(liftIO))
import Eventuria.Commons.System.SafeResponse
import Data.Text
import Data.Function ((&))
import Eventuria.GSD.CLI.Workflow.Steps
import Eventuria.GSD.Read.Model.Workspace
import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import Eventuria.GSD.CLI.UI.Greetings

import Eventuria.GSD.Monitoring.API.Client.Client (streamGsdEventByWorkspaceId,
                              streamGsdCommandByWorkspaceId,
                              streamGsdCommandResponseByWorkspaceId,
                              streamGsdValidationStateByWorkspaceId)
import Eventuria.GSD.Monitoring.API.Client.Dependencies
import Eventuria.Commons.Logger.Core


data MonitoringCommand = ListCommandsReceived
                       | ListCommandResponsesProduced
                       | ListEventsGenerated
                       | ListValidationStates


runMonitoringCommand :: forall stepType. Step stepType ->
                                         MonitoringCommand ->
                                         Dependencies ->
                                         Workspace ->
                                         Byline IO (Either StepError (Step stepType))
runMonitoringCommand currentStep monitoringCommand dependencies @ Dependencies { logger} Workspace {workspaceId} =
  (case monitoringCommand of
    ListCommandsReceived ->         displayCallResult currentStep  logger "Commands"                 (streamGsdCommandByWorkspaceId         dependencies workspaceId)
    ListCommandResponsesProduced -> displayCallResult currentStep  logger "Command Responses"        (streamGsdCommandResponseByWorkspaceId dependencies workspaceId)
    ListEventsGenerated ->          displayCallResult currentStep  logger "Events"                   (streamGsdEventByWorkspaceId           dependencies workspaceId)
    ListValidationStates ->         displayCallResult currentStep  logger "Validation State History" (streamGsdValidationStateByWorkspaceId dependencies workspaceId))

  where
    displayCallResult :: forall item stepType. Show item =>
                                                 Step stepType ->
                                                 Logger ->
                                                 String ->
                                                 IO( SafeResponse [Persisted item]) ->
                                                 Byline IO (Either StepError (Step stepType))
    displayCallResult currentStep logger itemName clientCall = do
      displayBeginningOfACommand
      sayLn $ fg white <> (text . pack) itemName
      safeResponse <- liftIO $ clientCall
      nextStep <- case safeResponse of
          Left error -> return $ Left $ StepError { currentStep, errorDescription = show error }
          Right persistedItems -> persistedItems & mapM (sayLn . displayItem) >> (return $ Right currentStep)
      displayEndOfACommand
      return nextStep

      where
        displayItem :: Show item => (Persisted item) -> Stylized
        displayItem PersistedItem {offset, item} =
          fg white <> "  "<>(text.pack.show) offset <> "- " <> fg green <> (text.pack.show) item

