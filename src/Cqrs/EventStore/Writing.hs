{-# LANGUAGE DuplicateRecordFields #-}

module Cqrs.EventStore.Writing where

import qualified Database.EventStore as EventStore
import qualified Data.Text as Text
import qualified Data.UUID.V4 as Uuid
import Control.Concurrent.Async (wait)


import Cqrs.Streams
import Control.Monad.IO.Class (MonadIO(..))


import Cqrs.EventStore.Context
import Data.Aeson

data EventStorePersisting itemToPersist = EventStoreWriting {
                                           context :: EventStoreContext,
                                           streamName :: EventStore.StreamName}

class ToJSON item => Persistable item where
  getItemName :: item -> String


persist :: Persistable item =>  EventStoreContext -> EventStore.StreamName ->  item -> IO (Either PersistenceFailure PersistResult)
persist Context { logger = logger,
                  credentials = credentials,
                  connection = connection } streamName itemToPersist =  do

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
