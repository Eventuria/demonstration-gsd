module CommandSourcing.Logger where
import qualified System.Log.Logger as LoggerUsed
import Control.Concurrent

type LoggerId = String
type ExecutableName = String
type LoggerMessage = String

data Logger = Logger { loggerId :: LoggerId , executableName :: ExecutableName }

initLogger :: Logger -> IO()
initLogger logger = do
  LoggerUsed.updateGlobalLogger (loggerId logger) $ LoggerUsed.setLevel LoggerUsed.INFO

logInfo :: Logger -> LoggerMessage -> IO ()
logInfo logger message = do
  threadId <- myThreadId
  LoggerUsed.infoM (loggerId logger) $ (executableName logger) ++ " - "++ (show threadId) ++ " - " ++ message