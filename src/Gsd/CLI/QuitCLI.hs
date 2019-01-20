{-# LANGUAGE OverloadedStrings #-}
module Gsd.CLI.QuitCLI where

import System.Console.Byline
import Control.Monad.IO.Class (MonadIO(liftIO))
import System.Exit (exitSuccess)

runQuitCLI :: Byline IO ()
runQuitCLI = do
  sayLn $ fg green <> "###############################################"
  sayLn $ fg green <> "You're leaving gsd, See you soon !! "
  sayLn $ fg green <> "###############################################"
  liftIO $ exitSuccess