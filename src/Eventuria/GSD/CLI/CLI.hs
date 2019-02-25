{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Eventuria.GSD.CLI.CLI where

import           Control.Monad
import           Prelude hiding (map)
                 
import           System.Console.Byline

import qualified Eventuria.GSD.CLI.UI.Workspaces            as WorkspacesUI (run)
import           Eventuria.GSD.CLI.UI.Greetings
import           Eventuria.GSD.CLI.Settings
import           Eventuria.GSD.CLI.UI.HealthChecking
import           Eventuria.GSD.CLI.Dependencies

execute :: Settings -> IO ()
execute = getDependencies              >=>
          waitTillHealthyDependencies  >=>
          greetings                    >=>
          void . runByline . WorkspacesUI.run







