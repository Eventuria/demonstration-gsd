{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Gsd.CLI.UI.Greetings  where

import System.Console.Byline


greetings :: Byline IO ()
greetings = do
  sayLn $ fg green <> "###############################################"
  sayLn $ fg green <> "||       Welcome to the gsd client !         ||"
  sayLn $ fg green <> "###############################################"


displayBeginningOfACommand ::  Byline IO ()
displayBeginningOfACommand = sayLn $ fg white <> "------------------------------------------"

displayEndOfACommand ::  Byline IO ()
displayEndOfACommand = sayLn $ fg white <> "------------------------------------------"