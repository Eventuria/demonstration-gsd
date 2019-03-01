{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Read.Subscribing where

import qualified Streamly.Prelude as S
import Control.Concurrent
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Database.EventStore as EventStore

import Eventuria.Commons.Logger.Core
import Control.Exception
import Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.EventStoreStream
import Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Dependencies
import Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import Data.Maybe
import Data.Aeson
import Eventuria.Libraries.PersistedStreamEngine.Interface.Streamable
import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import Eventuria.Adapters.Time.Core

subscribe :: Streamable stream monad item => EventStoreStream item -> stream monad (Either SomeException (Persisted item))
subscribe eventStoreStream @ EventStoreStream {dependencies = Dependencies { logger, credentials, connection },
                                               streamName} = do
  liftIO $ logInfo logger $ "subscribing to stream : " ++ show streamName

  result <- liftIO $ try (askForSubscription)

  case result of
    Left e @ SomeException {} -> do
               liftIO $ logInfo logger "subscription to stream failed - retrying..."
               liftIO $ threadDelay $ getInMsFromSeconds 5
               subscribe eventStoreStream
    Right subscription -> do
               liftIO $ logInfo logger $ "subscription enabled on stream " ++ show streamName
               loopNextEvent subscription
                where
                   loopNextEvent subscription = do
                      resolvedEvent <- liftIO $ EventStore.nextEvent subscription
                      liftIO $ logInfo logger $ "subscription triggered on " ++ show streamName
                                             ++ " with event > " ++ (show resolvedEvent)
                      (S.yield $ Right $ recordedEventToPersistedItem $ (EventStore.resolvedEventOriginal resolvedEvent))
                        <> loopNextEvent subscription
  where
     askForSubscription :: IO (EventStore.RegularSubscription EventStore.EventNumber)
     askForSubscription = do
        subscription <- EventStore.subscribe connection streamName EventStore.NoResolveLink Nothing
        EventStore.waitConfirmation subscription
        return subscription

subscribeOnOffset :: FromJSON item => EventStoreStream item -> Offset -> IO (Either SomeException (Persisted item))
subscribeOnOffset eventStoreStream @ EventStoreStream {dependencies = Dependencies { logger, credentials, connection },
                                                       streamName}
                  offset = do
  logInfo logger $ "subscribing to stream : " ++ show streamName ++ " on the offset " ++ (show offset)

  subscription <- EventStore.subscribeFrom
                    connection
                    streamName
                    EventStore.NoResolveLink (Just $ EventStore.rawEventNumber (fromInteger offset))
                    Nothing
                    Nothing
  result <- (try $ EventStore.waitConfirmation subscription )
  case result of
    Left e @ SomeException {} -> do
               logInfo logger "subscription to stream failed - retrying..."
               threadDelay $ getInMsFromSeconds 5
               subscribeOnOffset eventStoreStream offset
    Right _ -> do
            resolvedEvent <- EventStore.nextEvent subscription
            logInfo logger $ "subscription triggered on " ++ show streamName ++ " with event > " ++ (show resolvedEvent)
            return $ Right $ recordedEventToPersistedItem $ (EventStore.resolvedEventOriginal resolvedEvent)

recordedEventToPersistedItem :: FromJSON item => EventStore.RecordedEvent -> Persisted item
recordedEventToPersistedItem recordedEvent =
  PersistedItem { offset = toInteger $ EventStore.recordedEventNumber recordedEvent,
                  item = fromJust $ EventStore.recordedEventDataAsJson recordedEvent }