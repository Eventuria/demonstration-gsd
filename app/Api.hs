{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Api where

import Web.Scotty
import Prelude hiding (foldr)
import Logger.Core

import qualified Database.EventStore as EventStore
import Control.Exception
import Data.UUID
import Cqrs.Settings
import Streamly.Prelude
import Control.Monad.IO.Class (MonadIO(..))
import Plugins.GregYoungEventStore.Settings
import qualified Gsd.Gsd as Gsd
import Cqrs.Aggregate.StreamRepository

main :: IO ()
main = do
  let logger = Logger { loggerId = "[gsd.api]" , executableName = "api" }
  initLogger logger
  logInfo logger "[api] - Starting"

  bracket (EventStore.connect getEventStoreSettings getConnectionType)
         (\connection -> do EventStore.shutdown connection
                            EventStore.waitTillClosed connection)
         (\connection -> routing logger $ Gsd.getEventStoreStreamRepository Context {logger = logger, connection = connection , credentials = getCredentials})


routing :: Logger -> EventStoreStreamRepository  -> IO()
routing logger streamRepository @ StreamRepository { aggregateIdStream , getCommandStream }= scotty 3000 $ do
  get  "/health/liveness" $ do html "OK"
  get  "/readWorkspaceIds" $ do
    workspaceIds <- (liftIO $ foldr (:) [] $ Gsd.streamWorkspaceIds aggregateIdStream )
    liftIO $ logInfo logger $ "[api] - result > " ++ show workspaceIds
    json workspaceIds
  get  "/readCommands/:workspaceIdGiven" $ do
    workspaceIdString <- param "workspaceIdGiven"
    let workspaceIdOpt = fromString workspaceIdString
    case workspaceIdOpt of
                   Just (workspaceId) -> (liftIO $ foldr (:) []  $ Gsd.streamCommands getCommandStream workspaceId) >>= json
                   Nothing -> html "you've passed an invalid workspace id format"
  post "/requestCommand/" $ do
     liftIO $ logInfo logger "post /requestCommand/"
     command <- jsonData
     commands <- (liftIO $ Gsd.requestCommand getCommandStream aggregateIdStream command )
     json commands


