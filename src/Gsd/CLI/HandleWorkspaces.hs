{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Gsd.CLI.HandleWorkspaces (handleWorkspaces)where

import Prelude hiding (length)
import System.Console.Byline
import Data.Text
import Data.UUID.V4
import Data.UUID
import Control.Monad.IO.Class (MonadIO(liftIO))
import Gsd.Write.Client (sendCommand)
import Gsd.Write.Commands.Command
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
import Gsd.Read.Workspace
import Gsd.CLI.HandleWorkspace (handleWorkspace)

data HandleWorkspaceActions = CreateWorkspaceRequest  Text |
               DisplayWorkspaces Text |
               SelectAWorkspace Text |
               ExitScript Text deriving Show


displayHandleWorkspaceAction :: HandleWorkspaceActions -> Stylized
displayHandleWorkspaceAction (CreateWorkspaceRequest description) = fg cyan <> text description
displayHandleWorkspaceAction (DisplayWorkspaces description) = fg cyan <> text description
displayHandleWorkspaceAction (SelectAWorkspace description) = fg cyan <> text description
displayHandleWorkspaceAction (ExitScript description) = fg cyan <> text description


displayPersistedWorkspace :: Persisted Workspace -> Stylized
displayPersistedWorkspace PersistedItem {item = Workspace {workspaceId,workspaceName}} =
  fg cyan <> "Workspace (" <> (text $ toText workspaceId) <> " , " <> text workspaceName <> " )"


handleWorkspaceActions :: [HandleWorkspaceActions]
handleWorkspaceActions =
  [ CreateWorkspaceRequest "Create a workspace" ,
    DisplayWorkspaces  "Display workspaces",
    SelectAWorkspace  "Select a workspace",
    ExitScript  "Exit from the command client"]


handleWorkspaces :: BaseUrl -> BaseUrl -> Byline IO ()
handleWorkspaces writeApiUrl gsdReadApiUrl = do
  let menuConfig = banner "Handle your workspaces :" $ menu handleWorkspaceActions displayHandleWorkspaceAction
      prompt     = "please choose an action (provide the index) : "
      onError    = "please enter a valid index..."

  answer <- askWithMenuRepeatedly menuConfig prompt onError
  case answer of
    Match (CreateWorkspaceRequest description) -> do
        workspaceId <- liftIO $ nextRandom
        commandId <- liftIO $ nextRandom
        sayLn $ fg green <> "generating a new Workspace Id (" <> text (toText workspaceId) <> ") "
        sayLn $ fg green <> "generating a new Command Id (" <> text (toText commandId) <>") "
        workspaceName <- askUntil "Enter a workspace description : " Nothing atLeastThreeChars
        manager <- liftIO $ newManager defaultManagerSettings
        queryResult <- liftIO $ runClientM (sendCommand CreateWorkspace {commandId , workspaceId , workspaceName}) (mkClientEnv manager writeApiUrl)
        case queryResult of
          Left err -> do
            sayLn $ fg red <> "Error: " <>  (text . pack . show) err
            sayLn $ ""
            handleWorkspaces writeApiUrl gsdReadApiUrl
          Right persistenceResult -> do
            sayLn $ fg green <> "Workpace "<> (text . toText) workspaceId <> " successfully created !"
            sayLn $ ""
            handleWorkspaces writeApiUrl gsdReadApiUrl
    Match (DisplayWorkspaces description) -> do
      sayLn $ fg green <> "Listing all workspaces created : "
      manager <- liftIO $ newManager defaultManagerSettings
      liftIO $ S.withClientM streamWorkspaces (S.mkClientEnv manager gsdReadApiUrl) $ \e -> case e of
          Left err -> void $ runByline $ do
            sayLn $ fg red <> "Error: " <>  (text . pack . show) err
            sayLn $ ""
            handleWorkspaces writeApiUrl gsdReadApiUrl
          Right stream -> do
              runStream $ stream
                  & Streamly.Prelude.mapM (\PersistedItem { offset = offset, item = workspace} -> void $ runByline $ do
                    sayLn $ fg green <> (text . pack . show) workspace)
              void $ runByline $ handleWorkspaces writeApiUrl gsdReadApiUrl

    Match (SelectAWorkspace description) -> do
       manager <- liftIO $ newManager defaultManagerSettings
       liftIO $ S.withClientM streamWorkspaces (S.mkClientEnv manager gsdReadApiUrl) $ \e -> case e of
                 Left err -> void $ runByline $ do
                   sayLn $ fg red <> "Error: " <>  (text . pack . show) err
                   sayLn $ ""
                   handleWorkspaces writeApiUrl gsdReadApiUrl
                 Right stream -> void $ runByline $ do
                   workspaces <- liftIO $ stream & Streamly.Prelude.toList
                   let menuConfig = banner "Available workspaces :" $ menu workspaces displayPersistedWorkspace
                       prompt     = "please choose an action (provide the index) : "
                       onError    = "please enter a valid index..."
                   workspaceSelectedMatch <- askWithMenuRepeatedly menuConfig prompt onError
                   case workspaceSelectedMatch of
                    Match PersistedItem {item = workspaceSelected} -> do
                      sayLn $ fg green <> (text . pack . show) workspaceSelected <> " selected !"
                      sayLn $ ""
                      handleWorkspace writeApiUrl gsdReadApiUrl workspaceSelected handleWorkspaces
                    NoItems -> sayLn $ "unexpected answer"
                    Other x -> sayLn $ "unexpected answer"
    Match (ExitScript description) -> do
      sayLn $ fg green <> "See you soon !! "
      liftIO $ exitSuccess
    NoItems -> sayLn $ "unexpected answer"
    Other x -> sayLn $ "unexpected answer"

atLeastThreeChars :: Text -> IO (Either Stylized Text)
atLeastThreeChars input = return $
  if length input < 3
    then Left "3 characters minimum for a workspace please..."
    else Right input

