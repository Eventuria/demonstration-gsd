{-# LANGUAGE NamedFieldPuns #-}
module PersistedStreamEngine.Instances.EventStore.EventStoreClientManager where

import Logger.Core
import qualified Database.EventStore as EventStore
import PersistedStreamEngine.Instances.EventStore.EventStoreClientSettings
import Control.Exception


data EventStoreClientManager = EventStoreClientManager {
                                  logger :: Logger,
                                  credentials :: EventStore.Credentials,
                                  connection :: EventStore.Connection}


bracketEventStoreClientManager :: EventStoreClientSettings -> (EventStoreClientManager -> IO c) -> IO c
bracketEventStoreClientManager eventStoreClientSettings @ EventStoreClientSettings { loggerId ,
                                                      urlHost  ,
                                                      port     ,
                                                      username ,
                                                      password  } executionUnderTheBracket= do
    logger <- getLogger loggerId
    bracket ((EventStore.connect <$> getEventStoreSettings <*> getConnectionType) eventStoreClientSettings )
             (\connection -> do EventStore.shutdown connection
                                EventStore.waitTillClosed connection)
             (\connection ->  executionUnderTheBracket EventStoreClientManager {
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