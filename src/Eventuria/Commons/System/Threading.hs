{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Eventuria.Commons.System.Threading where

import Control.Exception
import Control.Concurrent
import Eventuria.Commons.Network.Core

catchingServerDownExceptionOnceAndThenDiscard :: ThreadId -> IO (ThreadId)
catchingServerDownExceptionOnceAndThenDiscard threadToNotifyThatExceptionReceived  =
  forkIO (
    catch
      (blockForever)
      (\ServerDownException -> do
          throwTo threadToNotifyThatExceptionReceived ServerDownException))
  where
    blockForever :: IO()
    blockForever = threadDelay 1000000000000000 >> blockForever

