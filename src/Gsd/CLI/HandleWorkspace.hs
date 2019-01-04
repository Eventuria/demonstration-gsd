{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Gsd.CLI.HandleWorkspace (handleWorkspace)where

import Prelude hiding (length)
import System.Console.Byline
import Data.Text
import Data.UUID.V4
import Data.UUID
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Gsd.Write.Client (sendCommand)
import Gsd.Monitoring.Client (streamGsdEventsByWorkspaceId, streamGsdCommandsByWorkspaceId)
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


type HandleWorkspaces = Clients -> Byline IO ()

data HandleWorkspaceActions = RenameWorkspaceRequest  Text
                            | ListCommandsReceived Text
                            | ListEventsGenerated Text
                            | HandleWorkspaces Text
                            | Quit Text deriving Show


displayHandleWorkspaceAction :: HandleWorkspaceActions -> Stylized
displayHandleWorkspaceAction (RenameWorkspaceRequest description) = fg cyan <> text description
displayHandleWorkspaceAction (ListCommandsReceived description) = fg cyan <> text description
displayHandleWorkspaceAction (ListEventsGenerated description) = fg cyan <> text description
displayHandleWorkspaceAction (HandleWorkspaces description) = fg cyan <> text description
displayHandleWorkspaceAction (Quit description) = fg cyan <> text description



handleWorkspaceActions :: [HandleWorkspaceActions]
handleWorkspaceActions =
  [ RenameWorkspaceRequest  "Rename Workspace" ,
    ListCommandsReceived "List Commands Received",
    ListEventsGenerated  "List Events Generated",
    HandleWorkspaces        "Work On Another Workspace",
    Quit                    "Quit"]


handleWorkspace :: Clients -> WorkspaceId -> WorkspaceName ->  HandleWorkspaces -> Byline IO ()
handleWorkspace clients @ Clients {writeApiUrl,gsdMonitoringApiUrl} workspaceIdSelected workspaceNameSelected handleWorkspaces = do
  let menuConfig = banner ("Available actions on the selected workspace : " <> fg green <> ((text . pack .show) $ workspaceNameSelected)) $ menu handleWorkspaceActions displayHandleWorkspaceAction
      prompt     = "please choose an action (provide the index) : "
      onError    = "please enter a valid index..."

  answer <- askWithMenuRepeatedly menuConfig prompt onError
  case answer of
    Match (RenameWorkspaceRequest description) -> do
      commandId <- liftIO $ nextRandom
      sayLn $ fg green <> "generating a new Command Id (" <> text (toText commandId) <>") "
      workspaceNewName <- askUntil "Enter a new workspace name : " Nothing atLeastThreeChars
      manager <- liftIO $ newManager defaultManagerSettings
      queryResult <- liftIO $ runClientM (sendCommand RenameWorkspace {commandId , workspaceId = workspaceIdSelected , workspaceNewName}) (mkClientEnv manager writeApiUrl)
      case queryResult of
          Left err -> do
            sayLn $ fg red <> "Error: " <>  (text . pack . show) err
            sayLn $ ""
            handleWorkspaces clients
          Right persistenceResult -> do
            sayLn $ fg green <> "Workpace successfully renamed !"
            sayLn $ ""
      handleWorkspace clients workspaceIdSelected workspaceNewName handleWorkspaces
    Match (ListCommandsReceived description) -> do
      manager <- liftIO $ newManager defaultManagerSettings
      liftIO $ S.withClientM (streamGsdCommandsByWorkspaceId workspaceIdSelected) (S.mkClientEnv manager gsdMonitoringApiUrl) $ \e -> case e of
         Left err -> void $ runByline $ do
           sayLn $ fg red <> "Error: " <>  (text . pack . show) err
           sayLn $ ""
           handleWorkspaces clients
         Right streamGsdEventsByWorkspaceId -> do
           runStream $ streamGsdEventsByWorkspaceId
                 & Streamly.Prelude.mapM (\persistedGsdCommand -> void $ runByline $ do
                   sayLn $ fg green <> (text . pack . show) persistedGsdCommand)
      handleWorkspace clients workspaceIdSelected workspaceNameSelected handleWorkspaces
    Match (ListEventsGenerated description) -> do
      manager <- liftIO $ newManager defaultManagerSettings
      liftIO $ S.withClientM (streamGsdEventsByWorkspaceId workspaceIdSelected) (S.mkClientEnv manager gsdMonitoringApiUrl) $ \e -> case e of
         Left err -> void $ runByline $ do
           sayLn $ fg red <> "Error: " <>  (text . pack . show) err
           sayLn $ ""
           handleWorkspaces clients
         Right streamGsdEventsByWorkspaceId -> do
           runStream $ streamGsdEventsByWorkspaceId
                 & Streamly.Prelude.mapM (\persistedGsdEvent -> void $ runByline $ do
                   sayLn $ fg green <> (text . pack . show) persistedGsdEvent)
      handleWorkspace clients workspaceIdSelected workspaceNameSelected handleWorkspaces
    Match (HandleWorkspaces description) -> do
      sayLn $ fg green <> (text . pack .show) description <> "selected "
      handleWorkspaces clients
    Match (Quit description) -> do
      sayLn $ fg green <> "See you soon !! "
      liftIO $ exitSuccess
    NoItems -> sayLn $ "unexpected answer"
    Other x -> sayLn $ "unexpected answer"


atLeastThreeChars :: Text -> IO (Either Stylized Text)
atLeastThreeChars input = return $
  if length input < 3
    then Left "3 characters minimum for a workspace please..."
    else Right input
