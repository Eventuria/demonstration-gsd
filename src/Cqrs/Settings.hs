{-# LANGUAGE OverloadedStrings #-}
module Cqrs.Settings where

import qualified Database.EventStore as EventStore


getCredentials :: EventStore.Credentials
getCredentials = EventStore.credentials "admin" "changeit"

getConnectionType :: EventStore.ConnectionType
getConnectionType = (EventStore.Static "127.0.0.1" 1113)

getEventStoreSettings :: EventStore.Settings
getEventStoreSettings = EventStore.defaultSettings