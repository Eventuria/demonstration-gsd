{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Gsd.CLI.QuitCLI where

import Gsd.CLI.Steps
import System.Console.Byline

runQuitCLI :: Byline IO (Either StepError (Step Quit))
runQuitCLI = do
  sayLn $ fg green <> "###############################################"
  sayLn $ fg green <> "You're leaving gsd, See you soon !! "
  sayLn $ fg green <> "###############################################"
  return $ Right QuitStep
