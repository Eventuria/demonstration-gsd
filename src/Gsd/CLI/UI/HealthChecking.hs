{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Gsd.CLI.UI.HealthChecking where

import Prelude hiding (map)
import System.Console.Byline
import Control.Monad (void)

import Data.Validation
import Gsd.CLI.Settings
import Gsd.CLI.Dependencies
import Data.Function ((&))
import Control.Concurrent
import Data.Text hiding (map)
import Time.Core
import Dependencies.Core

runHealthChecking :: Settings -> IO (Dependencies)
runHealthChecking settings = do
  (void $ runByline $ do
    sayLn $ fg green <> "###############################################"
    sayLn $ fg green <> "||          Service Health Checking          ||"
    sayLn $ fg green <> "###############################################")
  healthCheck (\cliDependencies  -> do
                  (void $ runByline $ do
                    sayLn $ fg white <> "------------------------------------------"
                    sayLn $ fg white <> " [" <> fg green <> "√" <> fg white <> "] Service is up and running"
                    sayLn $ fg white <> "------------------------------------------")
                  return cliDependencies)

  where
    healthCheck :: (Dependencies -> IO (Dependencies)) -> IO (Dependencies)
    healthCheck successHandling = do
      result <- retrieveDependencies settings
      validation
        (\errors -> do
            (void $ runByline $ do
              sayLn $ fg red <> "> The Service can't be up and running."
              sayLn $ fg red <> "> Some Dependencies are unhealthy :"
              errors & mapM_ (\UnhealthyDependency {name} ->
                  sayLn $ fg red <> "    [x] " <> fg cyan <> (text . pack) name)
              sayLn $ fg white <> "------------------------------------------"
              sayLn "Retrying in 10s"
              sayLn $ fg white <> "------------------------------------------")
            threadDelay $ getInMsFromSeconds 10
            healthCheck successHandling)
        successHandling result
