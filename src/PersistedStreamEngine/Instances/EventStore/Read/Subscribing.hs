{-# LANGUAGE NamedFieldPuns #-}

module PersistedStreamEngine.Instances.EventStore.Read.Subscribing where

import qualified Streamly.Prelude as S
import Control.Concurrent
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Database.EventStore as EventStore

import Logger.Core
import Control.Exception
import PersistedStreamEngine.Instances.EventStore.EventStoreStream
import PersistedStreamEngine.Instances.EventStore.EventStoreSettings
import PersistedStreamEngine.Interface.PersistedItem
import Data.Maybe
import Data.Aeson
import PersistedStreamEngine.Interface.Streamable

subscribe :: Streamable stream monad item => EventStoreStream item -> stream monad (Persisted item)
subscribe eventStoreStream @ EventStoreStream {settings = EventStoreSettings { logger, credentials, connection },
                                               streamName = streamName} = do
              liftIO $ logInfo logger $ "subscribing to stream : " ++ show streamName

              subscription <- liftIO $ EventStore.subscribe connection streamName EventStore.NoResolveLink Nothing
              result <- liftIO $ (try $ EventStore.waitConfirmation subscription )
              case result of
                Left e @ SomeException {} -> do
                           liftIO $ logInfo logger "subscription to stream failed - retrying..."
                           liftIO $ threadDelay (5 * 1000000) -- 5 seconds
                           subscribe eventStoreStream
                Right _ -> do
                           liftIO $ logInfo logger $ "subscription enabled on stream " ++ show streamName
                           loopNextEvent subscription where
                           loopNextEvent subscription = do
                              resolvedEvent <- liftIO $ EventStore.nextEvent subscription
                              liftIO $ logInfo logger $ "subscription triggered on " ++ show streamName ++ " with event > " ++ (show resolvedEvent)
                              (S.yield $ recordedEventToPersistedItem $ (EventStore.resolvedEventOriginal resolvedEvent)) <> loopNextEvent subscription

recordedEventToPersistedItem :: FromJSON item => EventStore.RecordedEvent -> Persisted item
recordedEventToPersistedItem recordedEvent =
  PersistedItem { offset = toInteger $ EventStore.recordedEventNumber recordedEvent,
                  item = fromJust $ EventStore.recordedEventDataAsJson recordedEvent }