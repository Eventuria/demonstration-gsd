{-# LANGUAGE NamedFieldPuns #-}
module PersistedStreamEngine.Instances.EventStore.EventStoreClientState where

import Logger.Core
import qualified Database.EventStore as EventStore
import PersistedStreamEngine.Instances.EventStore.EventStoreClientSettings
import Control.Exception


data EventStoreClientState = EventStoreClientState {
                                  logger :: Logger,
                                  credentials :: EventStore.Credentials,
                                  connection :: EventStore.Connection}


getState :: EventStoreClientSettings -> (EventStoreClientState -> IO c) -> IO c
getState eventStoreClientSettings @ EventStoreClientSettings { loggerId ,
                                                      urlHost  ,
                                                      port     ,
                                                      username ,
                                                      password  } executionUnderTheBracket= do
    logger <- getLogger loggerId
    bracket ((EventStore.connect <$> getEventStoreSettings <*> getConnectionType) eventStoreClientSettings )
             (\connection -> do EventStore.shutdown connection
                                EventStore.waitTillClosed connection)
             (\connection ->  executionUnderTheBracket EventStoreClientState {
                                                          logger,
                                                          credentials = getCredentials eventStoreClientSettings,
                                                          connection})

  where
    getCredentials :: EventStoreClientSettings -> EventStore.Credentials
    getCredentials EventStoreClientSettings {username,password} = EventStore.credentials username password

    getConnectionType :: EventStoreClientSettings -> EventStore.ConnectionType
    getConnectionType EventStoreClientSettings {urlHost,port} = (EventStore.Static urlHost port)

    getEventStoreSettings :: EventStoreClientSettings -> EventStore.Settings
    getEventStoreSettings service = EventStore.defaultSettings