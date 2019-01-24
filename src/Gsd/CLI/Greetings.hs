{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Gsd.CLI.Greetings  where

import System.Console.Byline


greetings :: Byline IO ()
greetings = do
  sayLn $ fg green <> "###############################################"
  sayLn $ fg green <> bold <> "Welcome to the gsd client !"
  sayLn $ fg green <> "###############################################"


displayBeginningOfACommand ::  Byline IO ()
displayBeginningOfACommand = sayLn $ fg white <> "------------------------------------------"

displayEndOfACommand ::  Byline IO ()
displayEndOfACommand = sayLn $ fg white <> "------------------------------------------"