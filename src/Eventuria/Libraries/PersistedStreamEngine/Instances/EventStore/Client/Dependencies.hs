{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Dependencies  where

import Eventuria.Commons.Logger.Core
import qualified Database.EventStore as EventStore
import Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Settings
import Control.Exception
import Control.Concurrent.Async (waitCatch)
import Eventuria.Commons.Dependencies.Core
import Data.List.NonEmpty
import Data.Bifunctor


data Dependencies = Dependencies {logger :: Logger,
                                  credentials :: EventStore.Credentials,
                                  connection :: EventStore.Connection}


getDependencies :: Settings ->
                   ExecutionUnderDependenciesAcquired Dependencies c ->
                   IO c
getDependencies eventStoreClientSettings @ Settings { loggerId ,
                                                      urlHost  ,
                                                      port     ,
                                                      username ,
                                                      password  }
                executionUnderDependenciesAcquired = do
  logger <- getLogger loggerId

  bracket
     (logInfo logger "acquiring connection" >>
      (EventStore.connect <$> getEventStoreSettings <*> getConnectionType) eventStoreClientSettings )
     (\connection -> do
       logInfo logger "releasing connection"
       catch
        (EventStore.shutdown connection >> EventStore.waitTillClosed connection)
        (\ex @SomeException {} -> case ex of
           ex | "Terminated \"Connection closed\"" == show ex ->
            return ()
           otherwise -> throw ex)
       logInfo logger "connection released")
     (\connection ->
       catch
          ( executionUnderDependenciesAcquired Dependencies {
                                            logger,
                                            credentials = getCredentials
                                            eventStoreClientSettings,
                                            connection})
          (\ex @SomeException {} -> case ex of
              ex | "Terminated \"Connection closed\"" == show ex -> do
                 logInfo logger "Terminated \"Connection closed\" rasised to a place I don't like...."
                 throw ex
              otherwise -> throw ex))

  where
    getCredentials :: Settings -> EventStore.Credentials
    getCredentials Settings {username,password} = EventStore.credentials username password

    getConnectionType :: Settings -> EventStore.ConnectionType
    getConnectionType Settings {urlHost,port} = (EventStore.Static urlHost port)

    getEventStoreSettings :: Settings -> EventStore.Settings
    getEventStoreSettings service = EventStore.defaultSettings


healthCheck :: Dependencies -> IO (Either (NonEmpty UnhealthyDependency) Dependencies)
healthCheck dependencies @ Dependencies{logger,connection,credentials}  =
   catch
     (do
      result <- call
      return $ bimap
          (\exception ->  (pure $ UnhealthyDependency {name = "eventStore", unhealthyReason = show exception}))
          (\right ->  dependencies) result)
     (\e @SomeException {} -> return $ Left $ (pure $ UnhealthyDependency {name = "eventStore", unhealthyReason = show e}))
  where call :: IO (Either SomeException (EventStore.BatchResult EventStore.Position))
        call = EventStore.readEventsForward
                   connection
                   EventStore.All
                   EventStore.positionStart
                   1
                   EventStore.NoResolveLink
                   (Just credentials) >>= waitCatch
