{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Write.Persisting where

import qualified Data.Text    as Text
import qualified Data.UUID.V4 as Uuid

import           Control.Concurrent.Async (wait)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Exception

import qualified Database.EventStore as EventStore

import           Eventuria.Commons.Logger.Core

import           Eventuria.Libraries.PersistedStreamEngine.Interface.Write.Writable
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Write.PersistenceResult
                 
import           Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.EventStoreStream
import           Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Dependencies


persist :: Writable item =>  EventStoreStream item -> item -> IO (Either SomeException PersistenceResult)
persist eventStoreStream @ EventStoreStream {  dependencies = Dependencies { logger, credentials, connection },
                                               streamName = streamName } itemToPersist =
    catch
    (do
        eventIdInEventStoreDomain <- liftIO $ Uuid.nextRandom
        let eventType               = EventStore.UserDefined $ Text.pack $ getItemName itemToPersist
            eventData               = EventStore.withJson itemToPersist
            eventInEventStoreDomain = EventStore.createEvent eventType (Just eventIdInEventStoreDomain) eventData

        writeResult <- liftIO $ EventStore.sendEvent
                connection
                streamName
                EventStore.anyVersion
                eventInEventStoreDomain
                (Just credentials) >>= wait

        return $ Right $ PersistenceResult $ toInteger $ EventStore.writeNextExpectedVersion writeResult)
    (\e @ SomeException {}  -> do
        liftIO $ logInfo logger $ "[persist] exception raised "  ++ show e
        return $ Left e)