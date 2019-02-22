{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module PersistedStreamEngine.Instances.EventStore.Client.Dependencies  where

import Eventuria.Commons.Logger.Core
import qualified Database.EventStore as EventStore
import PersistedStreamEngine.Instances.EventStore.Client.Settings
import Control.Exception
import Eventuria.Commons.DevOps.Core
import Control.Concurrent.Async (waitCatch)
import Eventuria.Commons.Dependencies.Core

data Dependencies = Dependencies {logger :: Logger,
                                  credentials :: EventStore.Credentials,
                                  connection :: EventStore.Connection}


retrieveDependencies :: Settings ->
                   ExecutionUnderDependenciesAcquired Dependencies c ->
                   ExecutionIfDependenciesAcquisitionFailed c ->
                   IO c
retrieveDependencies eventStoreClientSettings @ Settings { loggerId ,
                                                      urlHost  ,
                                                      port     ,
                                                      username ,
                                                      password  }
                executionUnderDependenciesAcquired
                executionIfDependenciesAcquisitionFailed = do
    logger <- getLogger loggerId
    catch
      (bracket
         (logInfo logger "acquiring connection" >>
          (EventStore.connect <$> getEventStoreSettings <*> getConnectionType) eventStoreClientSettings )
         (\connection -> do
           logInfo logger "releasing connection"
           EventStore.shutdown connection
           EventStore.waitTillClosed connection
           logInfo logger "connection released")
         (\connection ->
           (eventStoreClientHealthCheck logger connection) >>=
           either
              (\unhealthyReason -> executionIfDependenciesAcquisitionFailed
                                        (pure $ UnhealthyDependency {name = "eventStore", ..}))
              (\healthy -> executionUnderDependenciesAcquired
                              Dependencies {
                                 logger,
                                 credentials = getCredentials eventStoreClientSettings,
                                 connection})))
      (\ex @SomeException {} ->
        case ex of
          ex | "Terminated \"Connection closed\"" == show ex -> -- Issue with the way the event store haskell client
                                                              -- manages its exception. (exception raised is hidden
                                                              -- and the exception is thrown in an unexpected thread)
            executionIfDependenciesAcquisitionFailed
              (pure $ UnhealthyDependency {
                name = "eventStore",
                unhealthyReason = "EventStore service not reachable"})
          otherwise -> throw ex)

  where
    getCredentials :: Settings -> EventStore.Credentials
    getCredentials Settings {username,password} = EventStore.credentials username password

    getConnectionType :: Settings -> EventStore.ConnectionType
    getConnectionType Settings {urlHost,port} = (EventStore.Static urlHost port)

    getEventStoreSettings :: Settings -> EventStore.Settings
    getEventStoreSettings service = EventStore.defaultSettings

    eventStoreClientHealthCheck :: Logger -> EventStore.Connection -> IO (HealthCheckResult)
    eventStoreClientHealthCheck logger connection =
       catch
         (do -- Performing a call that will obligatory return a result if an event store instance is available
             EventStore.readEventsForward
                connection
                EventStore.All
                EventStore.positionStart
                1
                EventStore.NoResolveLink
                (Just $ getCredentials eventStoreClientSettings)
              >>= waitCatch
              >>= either
                    (\error -> return $ unhealthy "connection to the microservice eventStore failed")
                    (\right -> return healthy))
         (\e @SomeException {} -> return $ unhealthy "connection to the microservice eventStore failed")
