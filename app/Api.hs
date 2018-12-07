{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Api where

import Settings

import Web.Scotty
import Prelude hiding (foldr)
import Logger.Core

import qualified Database.EventStore as EventStore
import Control.Exception
import Data.UUID

import Streamly.Prelude
import Control.Monad.IO.Class (MonadIO(..))
import Plugins.GregYoungEventStore.EventStoreSettings
import qualified Gsd.GsdOverEventStore as Gsd

import Cqrs.Serialization.Aggregate.PersistenceResult ()

main :: IO ()
main = do
  let logger = Logger { loggerId = "[gsd.api]" , executableName = "api" }
  initLogger logger
  logInfo logger "[api] - Starting"

  bracket (EventStore.connect getEventStoreSettings getConnectionType)
         (\connection -> do EventStore.shutdown connection
                            EventStore.waitTillClosed connection)
         (\connection -> routing logger $ EventStoreSettings {logger = logger, credentials = getCredentials, connection = connection})


routing :: Logger -> EventStoreSettings  -> IO()
routing logger eventStoreSettings = scotty 3000 $ do
  get  "/health/liveness" $ do html "OK"
  get  "/readWorkspaceIds" $ do
    workspaceIds <- (liftIO $ foldr (:) [] $ Gsd.streamWorkspaceIds eventStoreSettings )
    liftIO $ logInfo logger $ "[api] - result > " ++ show workspaceIds
    json workspaceIds
  get  "/readCommands/:workspaceIdGiven" $ do
    workspaceIdString <- param "workspaceIdGiven"
    let workspaceIdOpt = fromString workspaceIdString
    case workspaceIdOpt of
                   Just (workspaceId) -> (liftIO $ foldr (:) []  $ Gsd.streamCommands eventStoreSettings workspaceId) >>= json
                   Nothing -> html "you've passed an invalid workspace id format"
  post "/requestCommand/" $ do
     liftIO $ logInfo logger "post /requestCommand/"
     command <- jsonData
     commands <- (liftIO $ Gsd.requestCommand eventStoreSettings command )
     json commands


