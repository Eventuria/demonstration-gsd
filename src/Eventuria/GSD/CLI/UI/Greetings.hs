{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Eventuria.GSD.CLI.UI.Greetings  where

import Control.Monad (void)
import System.Console.Byline
import Eventuria.GSD.CLI.Dependencies

greetings :: Dependencies -> IO (Dependencies)
greetings dependencies = do
  (void  $ runByline $ do
    sayLn $ fg green <> "###############################################"
    sayLn $ fg green <> "||       Welcome to the gsd client !         ||"
    sayLn $ fg green <> "###############################################")
  return dependencies

displayBeginningOfACommand ::  Byline IO ()
displayBeginningOfACommand = sayLn $ fg white <> "------------------------------------------"

displayEndOfACommand ::  Byline IO ()
displayEndOfACommand = sayLn $ fg white <> "------------------------------------------"