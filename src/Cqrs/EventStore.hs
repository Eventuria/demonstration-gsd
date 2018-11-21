{-# LANGUAGE OverloadedStrings #-}
module Cqrs.EventStore where

import qualified Database.EventStore as EventStore


getCredentials :: EventStore.Credentials
getCredentials = EventStore.credentials "admin" "changeit"