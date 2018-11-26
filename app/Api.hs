{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Api where

import Web.Scotty

import Cqrs.Logger
import Cqrs.EventStore.Writing
import Cqrs.Aggregate.Ids.AggregateIdStream
import Cqrs.EventStore.Streaming
import qualified Database.EventStore as EventStore
import Control.Exception
import Data.UUID
import Cqrs.Aggregate.Commands.CommandStream
import Cqrs.Settings
import Streamly
import Control.Monad.IO.Class (MonadIO(..))
import Cqrs.EventStore.Context
import Cqrs.Aggregate.Core

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
                   Just (workspaceId) -> do
                      let commandStream =  (getCommandStream eventStoreContext workspaceId)
                      (liftIO $ runStream $ streamAll commandStream) >>= json
                   Nothing -> html "you've passed an invalid workspace id format"
  post "/requestCommand/" $ do
     liftIO $ logInfo logger "post /requestCommand/"
     command <- jsonData
     let commandStream =  (getCommandStream eventStoreContext $ getAggregateId command)
     (liftIO $ persist commandStream command) >>= json
