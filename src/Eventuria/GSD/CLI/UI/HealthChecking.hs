{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Eventuria.GSD.CLI.UI.HealthChecking where

import Prelude hiding (map)
import System.Console.Byline

import Control.Monad (void)
import Control.Concurrent

import Data.Validation
import Data.Text hiding (map)
import Data.Function ((&))

import Eventuria.Adapters.Time.Core


import Eventuria.Commons.Dependencies.Core

import Eventuria.GSD.CLI.Dependencies

waitTillHealthyDependencies :: Dependencies -> IO (Dependencies)
waitTillHealthyDependencies cliDependencies = do
  (void $ runByline $ do
    sayLn $ fg green <> "###############################################"
    sayLn $ fg green <> "||          Service Health Checking          ||"
    sayLn $ fg green <> "###############################################")
  healthCheck (\whenHealthy  -> do
                  (void $ runByline $ do
                    sayLn $ fg white <> "------------------------------------------"
                    sayLn $ fg white <> " [" <> fg green <> "√" <> fg white <> "] Service is up and running"
                    sayLn $ fg white <> "------------------------------------------"))
  return cliDependencies

  where
    healthCheck :: (Healthy -> IO ()) -> IO ()
    healthCheck successHandling =
      checkDependenciesHealth cliDependencies >>=
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
          successHandling

