{-# LANGUAGE OverloadedStrings #-}
module Gsd.CLI.Greetings  where

import System.Console.Byline
import Gsd.Read.Workspace

greetings :: Byline IO ()
greetings = do
  sayLn $ fg green <> "###############################################"
  sayLn $ fg green <> bold <> "Welcome to the gsd client !"
  sayLn $ fg green <> "###############################################"

greetingOnWorkspace :: WorkspaceName -> Byline IO ()
greetingOnWorkspace workspaceName = do
  sayLn $ fg green <> "###############################################"
  sayLn $ fg green <> bold <> "Welcome on the Workspace : " <> text workspaceName
  sayLn $ fg green <> "###############################################"


displayBeginningOfACommand ::  Byline IO ()
displayBeginningOfACommand = sayLn $ fg green <> "------------------------------------------"

displayEndOfACommand ::  Byline IO ()
displayEndOfACommand = sayLn $ fg green <> "------------------------------------------"