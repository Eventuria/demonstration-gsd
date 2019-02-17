module Gsd.CLI.CLI where

import System.Console.Byline
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))

import qualified Gsd.CLI.UI.Workspaces as WorkspacesCLI (run)
import Gsd.CLI.UI.Greetings (greetings)

import Gsd.CLI.Settings
import Gsd.CLI.State

execute :: Settings -> IO ()
execute settings = void $ runByline $ do
  greetings
  state <- liftIO $ getState settings
  WorkspacesCLI.run state





