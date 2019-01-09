module Gsd.CLI.CLI where

import System.Console.Byline
import Control.Monad (void)


import Gsd.CLI.WorkspacesActions (workOnWorkspaces)
import Gsd.CLI.Greetings (greetings)
import Gsd.Clients


execute :: Clients -> IO ()
execute clients = void $ runByline $ do
  greetings
  workOnWorkspaces clients





