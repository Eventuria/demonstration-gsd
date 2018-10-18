{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Api where

import Web.Scotty
import CommandSourcing.CommandHandler
import CommandSourcing.Logger
import qualified CommandSourcing.CommandStream as CommandStream
import qualified CommandSourcing.WorkspaceStream as WorkspaceStream
import Data.Aeson.Encoding

import qualified Database.EventStore as EventStore
import Control.Exception
import Data.UUID


import qualified Control.Concurrent as Concurrent
import Control.Concurrent.Async

import Streamly
import qualified Streamly.Prelude as S
import Control.Concurrent
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function ((&))


main :: IO ()
main = do
  let logger = Logger { loggerId = "[gsd.api]" , executableName = "api" }
  initLogger logger
  logInfo logger "[api] - Starting"

  bracket (EventStore.connect EventStore.defaultSettings (EventStore.Static "127.0.0.1" 1113))
         (\connection -> do EventStore.shutdown connection
                            EventStore.waitTillClosed connection)
         (\connection -> routing logger connection)


routing :: Logger -> EventStore.Connection -> IO()
routing logger eventStoreConnection = scotty 3000 $ do
  get  "/health/liveness" $ do html "OK"
  get  "/workspaceIds" $ do
     (liftIO $ runStream $ WorkspaceStream.streamAll logger eventStoreConnection) >>= json
  get  "/:workspaceIdGiven/commands/read" $ do
    workspaceIdString <- param "workspaceIdGiven"
    let workspaceIdOpt = fromString workspaceIdString
    case workspaceIdOpt of
                   Just (workspaceId) ->
                      (liftIO $ runStream $ CommandStream.readForward eventStoreConnection workspaceId Nothing) >>= json
                   Nothing -> html "you've passed an invalid workspace id format"
  post "/requestCommand/" $ do
     liftIO $ logInfo logger "post /requestCommand/"
     command <- jsonData
     (liftIO $ runStream
             $ CommandStream.persist logger eventStoreConnection command) >>= json
