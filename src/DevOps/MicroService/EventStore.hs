{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module DevOps.MicroService.EventStore where

import qualified Database.EventStore as EventStore
import Data.ByteString
import Control.Exception
import DevOps.Core
import Control.Concurrent.Async (wait)

data EventStoreMicroService = EventStoreMicroService {
                                    urlHost :: String,
                                    port :: Int,
                                    username :: ByteString,
                                    password :: ByteString}

getCredentials :: EventStoreMicroService -> EventStore.Credentials
getCredentials EventStoreMicroService {username,password} = EventStore.credentials username password

getConnectionType :: EventStoreMicroService -> EventStore.ConnectionType
getConnectionType EventStoreMicroService {urlHost,port} = (EventStore.Static urlHost port)

getEventStoreSettings :: EventStoreMicroService -> EventStore.Settings
getEventStoreSettings service = EventStore.defaultSettings


instance MicroService EventStoreMicroService where

  healthCheck service = do
     catch connect (\e @SomeException {} -> return $ Unhealthy "connection to the microservice eventStore failed")

    where
      connect :: IO HealthCheckResult
      connect = do
         bracket (EventStore.connect (getEventStoreSettings service) (getConnectionType service))
           (\connection -> do EventStore.shutdown connection
                              EventStore.waitTillClosed connection)
           (\connection -> do
               result <- EventStore.readEventsForward
                          connection
                          EventStore.All
                          EventStore.positionStart
                          1
                          EventStore.NoResolveLink
                          (Just $ getCredentials service) >>= wait
               return Healthy)

