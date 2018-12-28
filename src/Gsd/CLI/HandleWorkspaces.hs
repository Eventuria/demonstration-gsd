{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Gsd.CLI.HandleWorkspaces (handleWorkspaces)where

import System.Console.Byline
import Data.Text
import Data.UUID.V4
import Data.UUID
import Control.Monad.IO.Class (MonadIO(liftIO))
import Gsd.Write.Client (sendCommand)
import Gsd.Write.Commands
import System.Exit (exitSuccess)
import Gsd.Read.Client (streamWorkspaces)

import Network.HTTP.Client (newManager, defaultManagerSettings)

import Servant.Client
import Data.Function ((&))
import Control.Monad (void)

import Streamly
import qualified Streamly.Prelude as Streamly.Prelude
import qualified Servant.Client.Streaming as S
import PersistedStreamEngine.Interface.PersistedItem

data HandleWorkspaceActions = CreateWorkspaceRequest  Text |
               DisplayWorkspaces Text |
               SelectAWorkspace Text |
               ExitScript Text deriving Show


displayItem :: HandleWorkspaceActions -> Stylized
displayItem (CreateWorkspaceRequest name) = fg cyan <> text name
displayItem (DisplayWorkspaces name) = fg cyan <> text name
displayItem (SelectAWorkspace name) = fg cyan <> text name
displayItem (ExitScript name) = fg cyan <> text name


items :: [HandleWorkspaceActions]
items = [ CreateWorkspaceRequest "Create a workspace" ,
          DisplayWorkspaces  "Display workspaces",
          SelectAWorkspace  "Select a workspace",
          ExitScript  "Exit from the command client"]


handleWorkspaces :: BaseUrl -> BaseUrl -> Byline IO ()
handleWorkspaces writeApiUrl gsdReadApiUrl = do
  let menuConfig = banner "Handle your workspaces :" $ menu items displayItem
      prompt     = "please select one (provide the index) : "
      onError    = "please enter a valid index..."

  answer <- askWithMenuRepeatedly menuConfig prompt onError
  case answer of
    Match (CreateWorkspaceRequest name) -> do
        workspaceId <- liftIO $ nextRandom
        commandId <- liftIO $ nextRandom
        sayLn $ fg green <> "generating a new Workspace Id (" <> text (toText workspaceId) <> ") "
        sayLn $ fg green <> "generating a new Command Id (" <> text (toText commandId) <>") "
        manager <- liftIO $ newManager defaultManagerSettings
        queryResult <- liftIO $ runClientM (sendCommand CreateWorkspace {commandId , workspaceId }) (mkClientEnv manager writeApiUrl)
        case queryResult of
          Left err -> do
            sayLn $ fg red <> "Error: " <>  text (pack $ show err)
            sayLn $ ""
            handleWorkspaces writeApiUrl gsdReadApiUrl
          Right persistenceResult -> do
            sayLn $ fg green <> "Workpace "<> text (toText workspaceId) <> " sucessfully created !"
            sayLn $ ""
            handleWorkspaces writeApiUrl gsdReadApiUrl
    Match (DisplayWorkspaces name) -> do
      sayLn $ fg green <> "Listing all workspaces created : "
      manager <- liftIO $ newManager defaultManagerSettings
      liftIO $ S.withClientM streamWorkspaces (S.mkClientEnv manager gsdReadApiUrl) $ \e -> case e of
          Left err -> void $ runByline $ do
            sayLn $ fg red <> "Error: " <>  text (pack $ show err)
            sayLn $ ""
            handleWorkspaces writeApiUrl gsdReadApiUrl
          Right stream -> do
              runStream $ stream
                  & Streamly.Prelude.mapM (\PersistedItem { offset = offset, item = workspace} -> void $ runByline $ do
                    sayLn $ fg green <> text (pack $ show workspace))
              void $ runByline $ handleWorkspaces writeApiUrl gsdReadApiUrl

    Match (SelectAWorkspace name) -> sayLn $ "you've picked: " <> fg blue <>  text name <> fg blue
    Match (ExitScript name) -> do
      sayLn $ fg green <> "See you soon !! "
      liftIO $ exitSuccess
    NoItems -> sayLn $ "unexpected answer"
    Other x -> sayLn $ "unexpected answer"