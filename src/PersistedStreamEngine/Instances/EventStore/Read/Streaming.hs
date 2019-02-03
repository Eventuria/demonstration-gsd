{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module PersistedStreamEngine.Instances.EventStore.Read.Streaming (
  streamAllSafe,
  streamFromOffsetSafe,
  streamFromOffset,
  streamAll,
  streamAllInfinitely) where

import Streamly
import qualified Streamly.Prelude as S
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent.Async (wait,waitCatch)
import GHC.Natural

import qualified Database.EventStore as EventStore
import qualified PersistedStreamEngine.Instances.EventStore.Read.Subscribing as EventStore.Subscribing
import PersistedStreamEngine.Interface.PersistedItem
import PersistedStreamEngine.Interface.Offset
import Logger.Core
import PersistedStreamEngine.Instances.EventStore.EventStoreStream
import PersistedStreamEngine.Instances.EventStore.EventStoreSettings
import Data.Aeson
import Data.Maybe
import PersistedStreamEngine.Interface.Streamable
import Control.Exception


data EvenStoreExceptionReason = ReadStreamDeleted
                        | ReadNotModified
                        | ReadError
                        | ReadAccessDenied  deriving Show

instance Exception EvenStoreExceptionReason

readStreamDeletedException,readNotModifiedException,readErrorException,readAccessDeniedException  :: SomeException
readStreamDeletedException        = toException ReadStreamDeleted
readNotModifiedException = toException ReadNotModified
readErrorException = toException ReadError
readAccessDeniedException = toException ReadAccessDenied


streamFromOffsetSafe :: StreamableSafe stream monad item =>
                      EventStoreStream item ->
                      Offset ->
                      stream monad (Either SomeException (Persisted item))
streamFromOffsetSafe eventStoreStream @ EventStoreStream {
                                       settings = EventStoreSettings { logger, credentials, connection },
                                       streamName = streamName } fromOffset = do

  liftIO $ logInfo logger $ "streaming [" ++ (show fromOffset) ++ "..] > " ++ show streamName

  let batchSize = 100

  commandFetched <- liftIO $ catch
        (EventStore.readEventsForward
                      connection
                      streamName
                      (EventStore.eventNumber $ naturalFromInteger fromOffset)
                      (fromInteger batchSize)
                      EventStore.NoResolveLink
                      (Just credentials) >>= waitCatch )
        (\e @ SomeException {}  -> return $ Left e)

  case commandFetched of
    Right (EventStore.ReadSuccess slices) -> do
      case (getPersistedItemsFromSlices slices) of
        persistedItems | (length persistedItems) == fromInteger batchSize ->
          (Right <$> S.fromList persistedItems) <> (streamFromOffsetSafe eventStoreStream $ fromOffset + batchSize)
        persistedItems -> Right <$> S.fromList persistedItems
    Right (EventStore.ReadNoStream) -> do
            liftIO $ logInfo logger $ "> " ++ show streamName ++ " is not found."
            S.fromList []
    Right (EventStore.ReadStreamDeleted e) -> return $ Left readStreamDeletedException
    Right (EventStore.ReadNotModified   )-> return $ Left readNotModifiedException
    Right (EventStore.ReadError e)        -> return $ Left readErrorException
    Right (EventStore.ReadAccessDenied e) -> return $ Left readAccessDeniedException
    Left (exception) -> return $ Left exception
  where
     getPersistedItemsFromSlices :: FromJSON item => EventStore.Slice t  -> [Persisted item]
     getPersistedItemsFromSlices slices = recordedEventToPersistedItem
                                             <$> EventStore.resolvedEventOriginal
                                             <$> EventStore.sliceEvents slices


streamAllSafe :: StreamableSafe stream monad item =>
                EventStoreStream item ->
                stream monad (Either SomeException (Persisted item))
streamAllSafe eventStoreStream = streamFromOffsetSafe eventStoreStream 0

streamAllInfinitely :: Streamable stream monad item => EventStoreStream item -> stream monad (Persisted item)
streamAllInfinitely eventStoreStream =
  (EventStore.Subscribing.subscribe eventStoreStream)
    `parallel` (streamAll eventStoreStream)

-- Deprecated
streamAll :: Streamable stream monad item =>
                EventStoreStream item ->
                stream monad (Persisted item)
streamAll eventStoreStream = streamFromOffset eventStoreStream 0


-- Deprecated
streamFromOffset :: Streamable stream monad item =>
                      EventStoreStream item ->
                      Offset ->
                      stream monad (Persisted item)

streamFromOffset eventStoreStream @ EventStoreStream {
                                       settings = EventStoreSettings { logger, credentials, connection },
                                       streamName = streamName } fromOffset = do
     liftIO $ logInfo logger $ "streaming [" ++ (show fromOffset) ++ "..] > " ++ show streamName

     let batchSize = 100

     asyncRead <- liftIO $ EventStore.readEventsForward
                      connection
                      streamName
                      (EventStore.eventNumber $ naturalFromInteger fromOffset)
                      (fromInteger batchSize)
                      EventStore.NoResolveLink
                      (Just credentials)
     commandFetched <- liftIO $ wait asyncRead
     case commandFetched of
          EventStore.ReadSuccess readResult -> do
              let persistedItems = recordedEventToPersistedItem
                                                    <$> EventStore.resolvedEventOriginal
                                                    <$> EventStore.sliceEvents readResult
              if (length persistedItems) == (fromInteger batchSize) then do
                  (S.fromList persistedItems) <>
                    (streamFromOffset eventStoreStream $ fromOffset + batchSize)
              else S.fromList persistedItems
          EventStore.ReadNoStream -> do
            liftIO $ logInfo logger $ "> " ++ show streamName ++ " is not found."
            S.fromList []
          e -> error $ "Read failure: " <> show e


recordedEventToPersistedItem :: FromJSON item => EventStore.RecordedEvent -> Persisted item
recordedEventToPersistedItem recordedEvent =
  PersistedItem { offset = toInteger $ EventStore.recordedEventNumber recordedEvent,
                  item = fromJust $ EventStore.recordedEventDataAsJson recordedEvent }