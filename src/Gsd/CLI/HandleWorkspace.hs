{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Gsd.CLI.HandleWorkspace (handleWorkspace)where

import Prelude hiding (length)
import System.Console.Byline
import Data.Text

import Control.Monad.IO.Class (MonadIO(liftIO))

import System.Exit (exitSuccess)

import Servant.Client

import Gsd.Read.Workspace

type HandleWorkspaces = BaseUrl -> BaseUrl -> Byline IO ()

data HandleWorkspaceActions = RenameWorkspace  Text
                            | DisplayCommandsSent Text
                            | DisplayEventsGenerated Text
                            | HandleWorkspaces Text
                            | ExitScript Text deriving Show


displayHandleWorkspaceAction :: HandleWorkspaceActions -> Stylized
displayHandleWorkspaceAction (RenameWorkspace description) = fg cyan <> text description
displayHandleWorkspaceAction (DisplayCommandsSent description) = fg cyan <> text description
displayHandleWorkspaceAction (DisplayEventsGenerated description) = fg cyan <> text description
displayHandleWorkspaceAction (HandleWorkspaces description) = fg cyan <> text description
displayHandleWorkspaceAction (ExitScript description) = fg cyan <> text description



handleWorkspaceActions :: [HandleWorkspaceActions]
handleWorkspaceActions =
  [ RenameWorkspace        "rename the workspace" ,
    DisplayCommandsSent     "Display the workspace commands sent",
    DisplayEventsGenerated  "Display the workspace event generated",
    HandleWorkspaces        "Handle another workspace",
    ExitScript              "Exit from the command client"]


handleWorkspace :: BaseUrl -> BaseUrl -> Workspace -> HandleWorkspaces -> Byline IO ()
handleWorkspace writeApiUrl gsdReadApiUrl workspaceSelected handleWorkspaces = do
  let menuConfig = banner ("Handle the selected workspace : " <> fg green <> ((text . pack .show) $ workspaceName workspaceSelected)) $ menu handleWorkspaceActions displayHandleWorkspaceAction
      prompt     = "please choose an action (provide the index) : "
      onError    = "please enter a valid index..."

  answer <- askWithMenuRepeatedly menuConfig prompt onError
  case answer of
    Match (RenameWorkspace description) -> do
      sayLn $ fg green <> (text . pack .show) description <> "selected "
      handleWorkspace writeApiUrl gsdReadApiUrl workspaceSelected handleWorkspaces
    Match (DisplayCommandsSent description) -> do
      sayLn $ fg green <> (text . pack .show) description <> "selected "
      handleWorkspace writeApiUrl gsdReadApiUrl workspaceSelected handleWorkspaces
    Match (DisplayEventsGenerated description) -> do
      sayLn $ fg green <> (text . pack .show) description <> "selected "
      handleWorkspace writeApiUrl gsdReadApiUrl workspaceSelected handleWorkspaces
    Match (HandleWorkspaces description) -> do
      sayLn $ fg green <> (text . pack .show) description <> "selected "
      handleWorkspaces writeApiUrl gsdReadApiUrl
    Match (ExitScript description) -> do
      sayLn $ fg green <> "See you soon !! "
      liftIO $ exitSuccess
    NoItems -> sayLn $ "unexpected answer"
    Other x -> sayLn $ "unexpected answer"



