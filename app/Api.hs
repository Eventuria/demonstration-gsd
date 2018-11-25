{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Api where

import Web.Scotty

import Cqrs.Logger
import qualified Cqrs.Aggregate.Commands.CommandStream as CommandStream
import Cqrs.Aggregate.Ids.AggregateIdStream
import Cqrs.EventStore.Streaming

import qualified Database.EventStore as EventStore
import Control.Exception
import Data.UUID

import Cqrs.Settings

import Streamly
import Control.Monad.IO.Class (MonadIO(..))
import Cqrs.EventStore.Context

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
  get  "/workspaceIds" $ do
     (liftIO $ runStream $ streamAll (getAggregateStream eventStoreContext) ) >>= json
  get  "/:workspaceIdGiven/commands/read" $ do
    workspaceIdString <- param "workspaceIdGiven"
    let workspaceIdOpt = fromString workspaceIdString
    case workspaceIdOpt of
                   Just (workspaceId) ->
                      (liftIO $ runStream $ CommandStream.readForward credentials connection workspaceId Nothing) >>= json
                   Nothing -> html "you've passed an invalid workspace id format"
  post "/requestCommand/" $ do
     liftIO $ logInfo logger "post /requestCommand/"
     command <- jsonData
     (liftIO $ CommandStream.persist eventStoreContext command) >>= json
