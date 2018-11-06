{-# LANGUAGE OverloadedStrings #-}
module Cqrs.EventStore where

import qualified Database.EventStore as EventStore


getCredentials :: Maybe EventStore.Credentials
getCredentials = Just $ EventStore.credentials "admin" "changeit"