module Gsd.CLI.CLI where

import System.Console.Byline
import Control.Monad (void)


import Gsd.CLI.HandleWorkspaces (handleWorkspaces)
import Gsd.CLI.Greetings (greetings)
import Gsd.Clients


execute :: Clients -> IO ()
execute clients = void $ runByline $ do
  greetings
  handleWorkspaces clients





