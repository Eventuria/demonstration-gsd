{-# LANGUAGE OverloadedStrings #-}
module Gsd.CLI.Greetings (greetings) where

import System.Console.Byline

greetings :: Byline IO ()
greetings = do
  sayLn $ fg green <> "###############################################"
  sayLn $ fg green <> bold <> "Welcome to the gsd client !"
  sayLn $ fg green <> "###############################################"