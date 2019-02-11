module Gsd.CLI.CLI where

import System.Console.Byline
import Control.Monad (void)


import qualified Gsd.CLI.WorkspacesCLI as WorkspacesCLI (run)
import Gsd.CLI.Greetings (greetings)
import Gsd.Clients


execute :: ClientsSetting -> IO ()
execute clients = void $ runByline $ do
  greetings
  WorkspacesCLI.run clients





