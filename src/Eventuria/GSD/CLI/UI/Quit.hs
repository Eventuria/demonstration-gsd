{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Eventuria.GSD.CLI.UI.Quit where

import Eventuria.GSD.CLI.Workflow.Steps
import System.Console.Byline

runQuitCLI :: Byline IO (Either StepError (Step Quit))
runQuitCLI = do
  sayLn $ fg green <> "###############################################"
  sayLn $ fg green <> "||    You're leaving gsd, See you soon !!    ||"
  sayLn $ fg green <> "###############################################"
  return $ Right QuitStep
