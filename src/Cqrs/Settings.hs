{-# LANGUAGE OverloadedStrings #-}
module Cqrs.Settings where

import qualified Database.EventStore as EventStore


getCredentials :: EventStore.Credentials
getCredentials = EventStore.credentials "admin" "changeit"