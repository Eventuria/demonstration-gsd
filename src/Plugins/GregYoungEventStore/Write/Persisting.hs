{-# LANGUAGE DuplicateRecordFields #-}

module Plugins.GregYoungEventStore.Write.Persisting where

import qualified Database.EventStore as EventStore
import qualified Data.Text as Text
import qualified Data.UUID.V4 as Uuid
import Control.Concurrent.Async (wait)


import Cqrs.Streams
import Control.Monad.IO.Class (MonadIO(..))
import Plugins.GregYoungEventStore.Stream

import Plugins.GregYoungEventStore.Settings
import Data.Aeson


class ToJSON item => Writable item where
  getItemName :: item -> String


persist :: Writable item =>  EventStoreStream item -> item -> IO (Either PersistenceFailure PersistResult)
persist eventStoreStream @ EventStoreStream {  context = Context { logger = logger,
                                                                   credentials = credentials,
                                                                   connection = connection },
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

    return $ Right $ PersistResult $ toInteger $ EventStore.writeNextExpectedVersion writeResult
