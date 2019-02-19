{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Gsd.CLI.CLI where

import Control.Monad (void)
import Prelude hiding (map)
import System.Console.Byline
import qualified Gsd.CLI.UI.Workspaces as WorkspacesUI (run)
import Gsd.CLI.UI.Greetings
import Gsd.CLI.Settings
import Gsd.CLI.UI.HealthChecking
import Control.Monad.IO.Class (MonadIO(liftIO))

execute :: Settings -> IO ()
execute settings = void $ runByline $ do
  cliSDependencies <- liftIO $ runHealthChecking settings
  greetings
  WorkspacesUI.run cliSDependencies







