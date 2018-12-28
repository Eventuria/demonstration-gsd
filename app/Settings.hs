{-# LANGUAGE OverloadedStrings #-}
module Settings where

import qualified Database.EventStore as EventStore

type ApiPort = Int

getCredentials :: EventStore.Credentials
getCredentials = EventStore.credentials "admin" "changeit"

getConnectionType :: EventStore.ConnectionType
getConnectionType = (EventStore.Static "127.0.0.1" 1113)

getEventStoreSettings :: EventStore.Settings
getEventStoreSettings = EventStore.defaultSettings


getWriteApiPort :: ApiPort
getWriteApiPort = 3000

getGsdMonitoringStreamingApiPort :: ApiPort
getGsdMonitoringStreamingApiPort = 3001

getGsdReadStreamingApiPort :: ApiPort
getGsdReadStreamingApiPort = 3002