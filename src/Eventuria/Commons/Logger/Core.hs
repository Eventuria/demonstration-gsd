{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.Commons.Logger.Core  where
import qualified System.Log.Logger as LoggerUsed
import Control.Concurrent

type LoggerId = String
type LoggerMessage = String

data Logger = Logger { loggerId :: LoggerId }

getLogger :: LoggerId -> IO (Logger)
getLogger loggerId  =
  LoggerUsed.updateGlobalLogger
    loggerId
    (LoggerUsed.setLevel LoggerUsed.INFO) >> return Logger {..}

logInfo :: Logger -> LoggerMessage -> IO ()
logInfo Logger {loggerId}  message = do
  threadId <- myThreadId
  LoggerUsed.infoM
    loggerId
    (loggerId ++ " - "++ (show threadId) ++ " - " ++ message)