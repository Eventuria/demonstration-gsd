{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Gsd.CLI.CLI where

import Prelude hiding (map)
import System.Console.Byline
import Control.Monad (void)
import qualified Gsd.CLI.UI.Workspaces as WorkspacesUI (run)
import Gsd.CLI.UI.Greetings
import Gsd.CLI.Settings
import Gsd.CLI.UI.HealthChecking

execute :: Settings -> IO ()
execute settings = do
  greetings
  cliState <- runHealthChecking settings
  WorkspacesUI.run cliState







