{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Api where

import Web.Scotty
import Prelude hiding (foldr)
import Cqrs.Logger

import qualified Database.EventStore as EventStore
import Control.Exception
import Data.UUID
import Cqrs.Settings
import Streamly.Prelude
import Control.Monad.IO.Class (MonadIO(..))
import Cqrs.EventStore.Context
import qualified Gsd.Gsd as Gsd


main :: IO ()
main = do
  let logger = Logger { loggerId = "[gsd.api]" , executableName = "api" }
  initLogger logger
  logInfo logger "[api] - Starting"

  bracket (EventStore.connect getEventStoreSettings getConnectionType)
         (\connection -> do EventStore.shutdown connection
                            EventStore.waitTillClosed connection)
         (\connection -> routing Context {logger = logger, connection = connection , credentials = getCredentials})


routing :: EventStoreContext -> IO()
routing eventStoreContext @ Context {logger = logger , connection = connection , credentials = credentials } = scotty 3000 $ do
  get  "/health/liveness" $ do html "OK"
  get  "/readWorkspaceIds" $ do
    workspaceIds <- (liftIO $ foldr (:) [] $ Gsd.streamWorkspaceIds eventStoreContext )
    liftIO $ logInfo logger $ "[api] - result > " ++ show workspaceIds
    json workspaceIds
  get  "/readCommands/:workspaceIdGiven" $ do
    workspaceIdString <- param "workspaceIdGiven"
    let workspaceIdOpt = fromString workspaceIdString
    case workspaceIdOpt of
                   Just (workspaceId) -> (liftIO $ foldr (:) []  $ Gsd.streamCommands eventStoreContext workspaceId) >>= json
                   Nothing -> html "you've passed an invalid workspace id format"
  post "/requestCommand/" $ do
     liftIO $ logInfo logger "post /requestCommand/"
     command <- jsonData
     commands <- (liftIO $ Gsd.requestCommand eventStoreContext command )
     json commands


