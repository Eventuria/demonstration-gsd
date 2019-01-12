module Gsd.CLI.CLI where

import System.Console.Byline
import Control.Monad (void)


import qualified Gsd.CLI.WorkspacesActions as WorkspacesActions (run)
import Gsd.CLI.Greetings (greetings)
import Gsd.Clients


execute :: Clients -> IO ()
execute clients = void $ runByline $ do
  greetings
  WorkspacesActions.run clients





