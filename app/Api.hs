{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Api where

import Web.Scotty
import CommandSourcing.CommandHandler
import qualified CommandSourcing.CommandStream as CommandStream
import qualified CommandSourcing.WorkspaceStream as WorkspaceStream
import Data.Aeson.Encoding
import CommandSourcing.EventStore (getEventStoreConnection)
import qualified Database.EventStore as EventStore
import Control.Exception
import System.Log.Logger
import Data.UUID
import Data.Conduit
import Conduit
import qualified Control.Concurrent as Concurrent
import Control.Concurrent.Async


main :: IO ()
main = do
         let logger = "[gsd.api]"
         updateGlobalLogger logger $ setLevel INFO
         infoM logger "[api] - Starting"
         bracket (EventStore.connect EventStore.defaultSettings (EventStore.Static "127.0.0.1" 1113))
                 (\connection -> do EventStore.shutdown connection
                                    EventStore.waitTillClosed connection)
                 (\connection -> routing connection)


routing :: EventStore.Connection -> IO()
routing eventStoreConnection = scotty 3000 $ do
                                          get  "/health/liveness" $ do html "OK"
                                          get  "/workspaceIds" $ do
                                             (liftIO $ runConduit $ WorkspaceStream.streamAll eventStoreConnection .| sinkList) >>= json
                                          get  "/:workspaceIdGiven/commands/read" $ do
                                            workspaceIdString <- param "workspaceIdGiven"
                                            let workspaceIdOpt = fromString workspaceIdString
                                            case workspaceIdOpt of
                                                           Just (workspaceId) ->
                                                              (liftIO $ runConduit
                                                                      $ yield (eventStoreConnection,workspaceId,0)
                                                                        .| CommandStream.readForward
                                                                        .| sinkList) >>= json
                                                           Nothing -> html "you've passed an invalid workspace id format"
                                          post "/requestCommand/" $ do
                                             let logger = "[gsd.api]"
                                             liftIO $ updateGlobalLogger logger $ setLevel INFO
                                             liftIO $ infoM logger "[api] - received command "
                                             command <- jsonData
                                             (liftIO $ runConduit
                                                     $ yield (eventStoreConnection,command)
                                                       .| CommandStream.persist
                                                       .| sinkList) >>= json
