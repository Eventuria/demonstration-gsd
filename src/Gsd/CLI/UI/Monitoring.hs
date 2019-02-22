{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
module Gsd.CLI.UI.Monitoring where

import System.Console.Byline
import Control.Monad.IO.Class (MonadIO(liftIO))
import Eventuria.Commons.System.SafeResponse
import Data.Text
import Data.Function ((&))
import Gsd.CLI.Workflow.Steps
import Gsd.Read.Model.Workspace
import PersistedStreamEngine.Interface.PersistedItem
import Gsd.CLI.UI.Greetings

import Gsd.Monitoring.API.Client.Client (streamGsdEventByWorkspaceId,
                              streamGsdCommandByWorkspaceId,
                              streamGsdCommandResponseByWorkspaceId,
                              streamGsdValidationStateByWorkspaceId)
import Gsd.Monitoring.API.Client.State
import Eventuria.Commons.Logger.Core


data MonitoringCommand = ListCommandsReceived
                       | ListCommandResponsesProduced
                       | ListEventsGenerated
                       | ListValidationStates


runMonitoringCommand :: forall stepType. Step stepType ->
                                         MonitoringCommand ->
                                         State ->
                                         Workspace ->
                                         Byline IO (Either StepError (Step stepType))
runMonitoringCommand currentStep monitoringCommand state @ State { logger} Workspace {workspaceId} =
  (case monitoringCommand of
    ListCommandsReceived ->         displayCallResult currentStep  logger "Commands"                 (streamGsdCommandByWorkspaceId         state workspaceId)
    ListCommandResponsesProduced -> displayCallResult currentStep  logger "Command Responses"        (streamGsdCommandResponseByWorkspaceId state workspaceId)
    ListEventsGenerated ->          displayCallResult currentStep  logger "Events"                   (streamGsdEventByWorkspaceId           state workspaceId)
    ListValidationStates ->         displayCallResult currentStep  logger "Validation State History" (streamGsdValidationStateByWorkspaceId state workspaceId))

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

