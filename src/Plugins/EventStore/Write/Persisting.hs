{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Plugins.EventStore.Write.Persisting where

import qualified Database.EventStore as EventStore
import qualified Data.Text as Text
import qualified Data.UUID.V4 as Uuid

import Control.Concurrent.Async (wait)
import Control.Monad.IO.Class (MonadIO(..))


import Cqrs.PersistedStreamEngine.Write.Writable
import Cqrs.PersistedStreamEngine.Write.PersistenceResult

import Plugins.EventStore.EventStoreStream
import Plugins.EventStore.EventStoreSettings


persist :: Writable item =>  EventStoreStream item -> item -> IO PersistenceResult
persist eventStoreStream @ EventStoreStream {  settings = EventStoreSettings { logger, credentials, connection },
                                               streamName = streamName } itemToPersist =  do

    eventIdInEventStoreDomain <- liftIO $ Uuid.nextRandom
    let eventType  = EventStore.UserDefined $ Text.pack $ getItemName itemToPersist
        eventData = EventStore.withJson itemToPersist
        eventInEventStoreDomain = EventStore.createEvent eventType (Just eventIdInEventStoreDomain) eventData

    writeResult <- liftIO $ EventStore.sendEvent
            connection
            streamName
            EventStore.anyVersion
            eventInEventStoreDomain
            (Just credentials) >>= wait

    return $ PersistenceSuccess $ toInteger $ EventStore.writeNextExpectedVersion writeResult
