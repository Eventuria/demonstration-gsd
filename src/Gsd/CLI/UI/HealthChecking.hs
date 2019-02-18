{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Gsd.CLI.UI.HealthChecking where

import Prelude hiding (map)
import System.Console.Byline
import Control.Monad (void)

import Data.Validation
import Gsd.CLI.Settings
import Gsd.CLI.State
import Data.Function ((&))
import Control.Concurrent
import Data.Text hiding (map)


runHealthChecking :: Settings -> IO (State)
runHealthChecking settings = do
  (void $ runByline $ do
    sayLn $ fg white <> "------------------------------------------"
    sayLn "Service Health Checking"
    sayLn $ fg white <> "------------------------------------------")
  healthCheck (\cliState  -> do
                  (void $ runByline $ do
                    sayLn $ fg green <> "The Service is healthy."
                    sayLn $ fg white <> "------------------------------------------")
                  return cliState)

  where
    healthCheck :: (State -> IO (State)) -> IO (State)
    healthCheck successHandling = do
      result <- getState settings
      validation
        (\errors -> do
            (void $ runByline $ do
              sayLn $ fg red <> "> The Service can't be up and running."
              sayLn $ fg red <> "> Some Dependencies are unhealthy :"
              errors & mapM_ (\ServiceUnhealthy {serviceName} ->
                  sayLn $ fg red <> "    [x] " <> fg cyan <> (text . pack) serviceName <> " Service")
              sayLn $ fg white <> "------------------------------------------"
              sayLn "Retrying in 10s"
              sayLn $ fg white <> "------------------------------------------")
            threadDelay (10 * 1000000) -- 5 seconds
            healthCheck successHandling)
        successHandling result
