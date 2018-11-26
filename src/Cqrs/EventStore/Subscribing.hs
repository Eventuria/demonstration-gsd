module Cqrs.EventStore.Subscribing where


import Streamly
import qualified Streamly.Prelude as S
import Control.Concurrent
import Control.Monad.IO.Class (MonadIO(liftIO))

import qualified Database.EventStore as EventStore

import Cqrs.Logger
import Control.Exception
import Cqrs.EventStore.Stream
import Cqrs.EventStore.Context
import Cqrs.EventStore.PersistedItem
import Data.Maybe
import Data.Aeson

subscribe :: (FromJSON item,
              IsStream stream,
              MonadIO (stream IO),
              Semigroup (stream IO (Persisted item)))  =>
                EventStoreStream item ->
                stream IO (Persisted item)
subscribe eventStoreStream @ EventStoreStream {
                                             context = Context { logger = logger,
                                                                 credentials = credentials,
                                                                 connection = connection },
                                             streamName = streamName} = do
              liftIO $ logInfo logger "subscribing to new aggregate created notifier"
              subscription <- liftIO $ EventStore.subscribe connection streamName True Nothing
              result <- liftIO $ (try $ EventStore.waitConfirmation subscription )
              case result of
                Left e @ SomeException {} -> do
                           liftIO $ logInfo logger "subscription to new aggregate created stream failed!"
                           liftIO $ threadDelay (5 * 1000000) -- 5 seconds
                           subscribe eventStoreStream
                Right _ -> do
                           liftIO $ logInfo logger "subscription started for commands handlers"
                           loopNextEvent subscription where
                           loopNextEvent subscription = do
                              resolvedEvent <- liftIO $ EventStore.nextEvent subscription
                              liftIO $ logInfo logger $ "new aggregate created stream event triggered : " ++ (show resolvedEvent)
                              (S.yield $ recordedEventToPersistedItem $ (EventStore.resolvedEventOriginal resolvedEvent)) <> loopNextEvent subscription

recordedEventToPersistedItem :: FromJSON item => EventStore.RecordedEvent -> Persisted item
recordedEventToPersistedItem recordedEvent =
  PersistedItem { offset = toInteger $ EventStore.recordedEventNumber recordedEvent,
                  item = fromJust $ EventStore.recordedEventDataAsJson recordedEvent }