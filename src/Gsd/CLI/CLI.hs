module Gsd.CLI.CLI where

import System.Console.Byline
import Control.Monad (void)
import Servant.Client

import Gsd.CLI.HandleWorkspaces (handleWorkspaces)
import Gsd.CLI.Greetings (greetings)

execute :: BaseUrl -> BaseUrl -> IO ()
execute writeApiUrl gsdReadApi = void $ runByline $ do
  greetings
  handleWorkspaces writeApiUrl gsdReadApi





