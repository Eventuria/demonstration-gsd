{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Eventuria.GSD.CLI.CLI where

import Control.Monad (void)
import Prelude hiding (map)
import System.Console.Byline
import qualified Eventuria.GSD.CLI.UI.Workspaces as WorkspacesUI (run)
import Eventuria.GSD.CLI.UI.Greetings
import Eventuria.GSD.CLI.Settings
import Eventuria.GSD.CLI.UI.HealthChecking
import Control.Monad.IO.Class (MonadIO(liftIO))

execute :: Settings -> IO ()
execute settings = void $ runByline $ do
  cliSDependencies <- liftIO $ runHealthChecking settings
  greetings
  WorkspacesUI.run cliSDependencies







