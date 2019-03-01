{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Read.Querying where

import           Control.Concurrent.Async (wait)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Exception

import           Data.Maybe
import           Data.Aeson

import qualified Database.EventStore as EventStore

import           Eventuria.Commons.Logger.Core

import           Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.EventStoreStream
import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import           Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Dependencies
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Offset

isStreamNotFound :: EventStoreStream item -> IO (Either SomeException Bool)
isStreamNotFound EventStoreStream { dependencies = Dependencies { logger, credentials, connection },
                                    streamName} =
  catch
   (do
      asyncRead <- EventStore.readEventsForward
                    connection
                    streamName
                    EventStore.streamStart
                    1
                    EventStore.NoResolveLink
                    (Just credentials)
      commandFetched <- wait asyncRead
      return $ Right $ case commandFetched of
            EventStore.ReadNoStream -> True
            _ -> False)
   (\e @ SomeException {}  -> do
      liftIO $ logInfo logger $ "[is.stream.not.found] exception raised "  ++ show e
      return $ Left e)

retrieveLast :: FromJSON item => EventStoreStream item -> IO( Either SomeException (Maybe (Persisted item)))
retrieveLast EventStoreStream { dependencies = Dependencies { logger, credentials, connection },
                                streamName} =
  catch
   (do
      let resolveLinkTos = False

      readResult <- EventStore.readEvent
                  connection
                  streamName
                  EventStore.streamEnd
                  EventStore.NoResolveLink
                  (Just credentials) >>= wait
      return $ Right $ case readResult of
        EventStore.ReadSuccess EventStore.ReadEvent {..} -> do
           Just $ recordedEventToPersistedItem (toInteger $ readEventNumber) readEventResolved
        EventStore.ReadNoStream ->
           Nothing
        e -> error $ show e)
   (\e @ SomeException {}  -> do
          liftIO $ logInfo logger $ "[retrieve.last] exception raised "  ++ show e
          return $ Left e)


recordedEventToPersistedItem :: FromJSON item => Offset -> EventStore.ResolvedEvent -> Persisted item
recordedEventToPersistedItem offset readEventResolved =
  PersistedItem { offset = offset,
                  item = fromJust $ EventStore.resolvedEventDataAsJson readEventResolved }

