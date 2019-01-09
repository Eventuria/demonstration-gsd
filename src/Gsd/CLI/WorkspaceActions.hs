{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Gsd.CLI.WorkspaceActions (workOnWorkspace)where

import Prelude hiding (length)
import System.Console.Byline
import Data.Text
import Data.UUID.V4
import Data.UUID
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Gsd.Write.Client (sendCommand)
import Gsd.Monitoring.Client (streamGsdEventByWorkspaceId,
                              streamGsdCommandByWorkspaceId,
                              streamGsdCommandResponseByWorkspaceId,
                              streamGsdValidationStateByWorkspaceId)
import System.Exit (exitSuccess)
import Gsd.Clients
import Servant.Client
import Gsd.Write.Commands.Command
import Gsd.Read.Workspace
import Gsd.Write.Core
import qualified Servant.Client.Streaming as S
import qualified Streamly.Prelude as Streamly.Prelude
import Streamly
import Data.Function ((&))
import Control.Monad (void)
import Gsd.Read.Client (streamGoal)

type WorkOnWorkspaces = Clients -> Byline IO ()

data WorkspaceActions = RenameWorkspaceAction  Text
                            | SetNewGoalAction  Text
                            | ListGoals  Text
                            | ListCommandsReceived Text
                            | ListCommandResponseProduced Text
                            | ListEventsGenerated Text
                            | ListValidationStateHistory Text
                            | WorkOnWorkspaces Text
                            | Quit Text deriving Show


workOnWorkspace :: Clients -> WorkspaceId -> WorkspaceName ->  WorkOnWorkspaces -> Byline IO ()
workOnWorkspace clients @ Clients {writeApiUrl,gsdMonitoringApiUrl,gsdReadApiUrl} workspaceIdSelected workspaceNameSelected workOnWorkspaces = do
  let menuConfig = banner ("Available actions on the selected workspace : " <> fg green <> ((text . pack .show) $ workspaceNameSelected)) $ menu workspaceActions stylizeAction
      prompt     = "please choose an action (provide the index) : "
      onError    = "please enter a valid index..."

  answer <- askWithMenuRepeatedly menuConfig prompt onError
  case answer of
    Match (RenameWorkspaceAction description) -> do
      commandId <- liftIO $ nextRandom
      sayLn $ fg green <> "generating a new Command Id (" <> text (toText commandId) <>") "
      workspaceNewName <- askUntil "Enter a new workspace name : " Nothing atLeastThreeChars
      manager <- liftIO $ newManager defaultManagerSettings
      queryResult <- liftIO $ runClientM (sendCommand RenameWorkspace {commandId , workspaceId = workspaceIdSelected , workspaceNewName}) (mkClientEnv manager writeApiUrl)
      case queryResult of
          Left err -> do
            sayLn $ fg red <> "Error: " <>  (text . pack . show) err
            sayLn $ ""
            workOnWorkspaces clients
          Right persistenceResult -> do
            sayLn $ fg green <> "Rename Workspace Command successfully sent !"
            sayLn $ ""
      workOnWorkspace clients workspaceIdSelected workspaceNewName workOnWorkspaces
    Match (SetNewGoalAction description) -> do
          commandId <- liftIO $ nextRandom
          sayLn $ fg green <> "generating a new Command Id (" <> text (toText commandId) <>") "
          goalId <- liftIO $ nextRandom
          sayLn $ fg green <> "generating a new goal Id (" <> text (toText goalId) <>") "
          goalDescription <- askUntil "Enter a goal description : " Nothing atLeastThreeChars
          manager <- liftIO $ newManager defaultManagerSettings
          queryResult <- liftIO $ runClientM (sendCommand SetGoal {commandId , workspaceId = workspaceIdSelected , goalId, goalDescription}) (mkClientEnv manager writeApiUrl)
          case queryResult of
              Left err -> do
                sayLn $ fg red <> "Error: " <>  (text . pack . show) err
                sayLn $ ""
                workOnWorkspaces clients
              Right persistenceResult -> do
                sayLn $ fg green <> "Set Goal Command successfully sent !"
                sayLn $ ""
          workOnWorkspace clients workspaceIdSelected workspaceNameSelected workOnWorkspaces
    Match (ListGoals description) -> do
          sayLn $ fg green <> "Listing goals set : "
          manager <- liftIO $ newManager defaultManagerSettings
          liftIO $ S.withClientM (streamGoal workspaceIdSelected)(S.mkClientEnv manager gsdReadApiUrl) $ \e -> case e of
              Left err -> void $ runByline $ do
                sayLn $ fg red <> "Error: " <>  (text . pack . show) err
                sayLn $ ""
                workOnWorkspaces clients
              Right stream -> do
                  runStream $ stream
                      & Streamly.Prelude.mapM (\goal -> void $ runByline $ do
                        sayLn $ fg green <> (text . pack . show) goal)
                  void $ runByline $ workOnWorkspace clients workspaceIdSelected workspaceNameSelected workOnWorkspaces
    Match (ListCommandsReceived description) -> do
      manager <- liftIO $ newManager defaultManagerSettings
      liftIO $ S.withClientM (streamGsdCommandByWorkspaceId workspaceIdSelected) (S.mkClientEnv manager gsdMonitoringApiUrl) $ \e -> case e of
         Left err -> void $ runByline $ do
           sayLn $ fg red <> "Error: " <>  (text . pack . show) err
           sayLn $ ""
           workOnWorkspaces clients
         Right streamGsdCommandByWorkspaceId -> do
           runStream $ streamGsdCommandByWorkspaceId
                 & Streamly.Prelude.mapM (\persistedGsdCommand -> void $ runByline $ do
                   sayLn $ fg green <> (text . pack . show) persistedGsdCommand)
      workOnWorkspace clients workspaceIdSelected workspaceNameSelected workOnWorkspaces
    Match (ListCommandResponseProduced description) -> do
      manager <- liftIO $ newManager defaultManagerSettings
      liftIO $ S.withClientM (streamGsdCommandResponseByWorkspaceId workspaceIdSelected) (S.mkClientEnv manager gsdMonitoringApiUrl) $ \e -> case e of
         Left err -> void $ runByline $ do
           sayLn $ fg red <> "Error: " <>  (text . pack . show) err
           sayLn $ ""
           workOnWorkspaces clients
         Right streamGsdCommandResponseByWorkspaceId -> do
           runStream $ streamGsdCommandResponseByWorkspaceId
                 & Streamly.Prelude.mapM (\persistedGsdCommandResponse -> void $ runByline $ do
                   sayLn $ fg green <> (text . pack . show) persistedGsdCommandResponse)
      workOnWorkspace clients workspaceIdSelected workspaceNameSelected workOnWorkspaces
    Match (ListEventsGenerated description) -> do
      manager <- liftIO $ newManager defaultManagerSettings
      liftIO $ S.withClientM (streamGsdEventByWorkspaceId workspaceIdSelected) (S.mkClientEnv manager gsdMonitoringApiUrl) $ \e -> case e of
         Left err -> void $ runByline $ do
           sayLn $ fg red <> "Error: " <>  (text . pack . show) err
           sayLn $ ""
           workOnWorkspaces clients
         Right streamGsdEventsByWorkspaceId -> do
           runStream $ streamGsdEventsByWorkspaceId
                 & Streamly.Prelude.mapM (\persistedGsdEvent -> void $ runByline $ do
                   sayLn $ fg green <> (text . pack . show) persistedGsdEvent)
      workOnWorkspace clients workspaceIdSelected workspaceNameSelected workOnWorkspaces
    Match (ListValidationStateHistory description) -> do
          manager <- liftIO $ newManager defaultManagerSettings
          liftIO $ S.withClientM (streamGsdValidationStateByWorkspaceId workspaceIdSelected) (S.mkClientEnv manager gsdMonitoringApiUrl) $ \e -> case e of
             Left err -> void $ runByline $ do
               sayLn $ fg red <> "Error: " <>  (text . pack . show) err
               sayLn $ ""
               workOnWorkspaces clients
             Right streamGsdEventsByWorkspaceId -> do
               runStream $ streamGsdEventsByWorkspaceId
                     & Streamly.Prelude.mapM (\persistedValidationState -> void $ runByline $ do
                       sayLn $ fg green <> (text . pack . show) persistedValidationState)
          workOnWorkspace clients workspaceIdSelected workspaceNameSelected workOnWorkspaces
    Match (WorkOnWorkspaces description) -> do
      sayLn $ fg green <> (text . pack .show) description <> "selected "
      workOnWorkspaces clients
    Match (Quit description) -> do
      sayLn $ fg green <> "See you soon !! "
      liftIO $ exitSuccess
    NoItems -> sayLn $ "unexpected answer"
    Other x -> sayLn $ "unexpected answer"
  where

    workspaceActions :: [WorkspaceActions]
    workspaceActions =
      [ RenameWorkspaceAction       "Rename Workspace" ,
        SetNewGoalAction            "Set A New Goal",
        ListGoals                   "List Goals",
        WorkOnWorkspaces            "Work On Another Workspace",
        ListCommandsReceived        "Infra-Monitoring - List Commands Received",
        ListCommandResponseProduced "Infra-Monitoring - List Command Responses Produced",
        ListEventsGenerated         "Infra-Monitoring - List Events Generated",
        ListValidationStateHistory  "Infra-Monitoring - List Validation State History",
        Quit                        "Quit"]

    stylizeAction :: WorkspaceActions -> Stylized
    stylizeAction (RenameWorkspaceAction description) = fg cyan <> text description
    stylizeAction (SetNewGoalAction description) = fg cyan <> text description
    stylizeAction (ListGoals description) = fg cyan <> text description
    stylizeAction (ListCommandsReceived description) = fg cyan <> text description
    stylizeAction (ListCommandResponseProduced description) = fg cyan <> text description
    stylizeAction (ListEventsGenerated description) = fg cyan <> text description
    stylizeAction (ListValidationStateHistory description) = fg cyan <> text description
    stylizeAction (WorkOnWorkspaces description) = fg cyan <> text description
    stylizeAction (Quit description) = fg cyan <> text description


    atLeastThreeChars :: Text -> IO (Either Stylized Text)
    atLeastThreeChars input = return $
      if length input < 3
        then Left "3 characters minimum for a workspace please..."
        else Right input
