{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Read.Streaming (
  streamFromRangeInclusive,
  streamFromOffsetInclusive,
  streamAll,
  streamAllInfinitely) where

import           GHC.Natural

import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Concurrent.Async (waitCatch)
import           Control.Exception

import           Data.Maybe
import           Data.Aeson


import           Streamly
import qualified Streamly.Prelude as S

import qualified Database.EventStore as EventStore

import           Eventuria.Commons.Logger.Core

import qualified Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Read.Subscribing as EventStore.Subscribing
import           Eventuria.Libraries.PersistedStreamEngine.Interface.PersistedItem
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import           Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.EventStoreStream
import           Eventuria.Libraries.PersistedStreamEngine.Instances.EventStore.Client.Dependencies
import           Eventuria.Libraries.PersistedStreamEngine.Interface.Streamable


streamFromRangeInclusive :: Streamable stream monad item =>
                      EventStoreStream item ->
                      Offset ->
                      Offset ->
                      stream monad (Either SomeException (Persisted item))
streamFromRangeInclusive eventStoreStream @ EventStoreStream {
                                       clientDependencies = Dependencies { logger, credentials, connection },
                                       streamName = streamName }
                fromOffset
                toOffset = do

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
        (\e @ SomeException {}  -> do
            liftIO $ logInfo logger $ "[stream.from.offset] exception raised "  ++ show e
            return $ Left e)

  case commandFetched of
    Right (EventStore.ReadSuccess slices) -> do
      case (filterBelowInclusive
              (getPersistedItemsFromSlices slices)
              toOffset) of
        persistedItems | (length persistedItems) == fromInteger batchSize ->
          (Right <$> S.fromList persistedItems) <> (streamFromOffsetInclusive eventStoreStream $ fromOffset + batchSize)
        persistedItems -> Right <$> S.fromList persistedItems
    Right (EventStore.ReadNoStream) -> do
            liftIO $ logInfo logger $ "> " ++ show streamName ++ " is not found."
            S.fromList []
    Right (EventStore.ReadStreamDeleted e) -> return $ Left readStreamDeletedException
    Right (EventStore.ReadNotModified   )-> return $ Left readNotModifiedException
    Right (EventStore.ReadError e)        -> return $ Left readErrorException
    Right (EventStore.ReadAccessDenied e) -> return $ Left readAccessDeniedException
    Left (exception) -> do
      liftIO $ logInfo logger $ "[stream.from.offset] exception propagated "  ++ show exception
      return $ Left exception
  where
     filterBelowInclusive :: [Persisted item] -> Offset -> [Persisted item]
     filterBelowInclusive items toOffset = filter (\PersistedItem {offset} -> offset <= toOffset ) items

     getPersistedItemsFromSlices :: FromJSON item => EventStore.Slice t  -> [Persisted item]
     getPersistedItemsFromSlices slices = recordedEventToPersistedItem
                                             <$> EventStore.resolvedEventOriginal
                                             <$> EventStore.sliceEvents slices


streamFromOffsetInclusive :: Streamable stream monad item =>
                      EventStoreStream item ->
                      Offset ->
                      stream monad (Either SomeException (Persisted item))
streamFromOffsetInclusive eventStoreStream @ EventStoreStream {
                                       clientDependencies = Dependencies { logger, credentials, connection },
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
        (\e @ SomeException {}  -> do
            liftIO $ logInfo logger $ "[stream.from.offset] exception raised "  ++ show e
            return $ Left e)

  case commandFetched of
    Right (EventStore.ReadSuccess slices) -> do
      case (getPersistedItemsFromSlices slices) of
        persistedItems | (length persistedItems) == fromInteger batchSize ->
          (Right <$> S.fromList persistedItems) <> (streamFromOffsetInclusive eventStoreStream $ fromOffset + batchSize)
        persistedItems -> Right <$> S.fromList persistedItems
    Right (EventStore.ReadNoStream) -> do
            liftIO $ logInfo logger $ "> " ++ show streamName ++ " is not found."
            S.fromList []
    Right (EventStore.ReadStreamDeleted e) -> return $ Left readStreamDeletedException
    Right (EventStore.ReadNotModified   )-> return $ Left readNotModifiedException
    Right (EventStore.ReadError e)        -> return $ Left readErrorException
    Right (EventStore.ReadAccessDenied e) -> return $ Left readAccessDeniedException
    Left (exception) -> do
      liftIO $ logInfo logger $ "[stream.from.offset] exception propagated "  ++ show exception
      return $ Left exception
  where
     getPersistedItemsFromSlices :: FromJSON item => EventStore.Slice t  -> [Persisted item]
     getPersistedItemsFromSlices slices = recordedEventToPersistedItem
                                             <$> EventStore.resolvedEventOriginal
                                             <$> EventStore.sliceEvents slices


streamAll :: Streamable stream monad item =>
                EventStoreStream item ->
                stream monad (Either SomeException (Persisted item))
streamAll eventStoreStream = streamFromOffsetInclusive eventStoreStream 0

streamAllInfinitely :: Streamable stream monad item =>
                          EventStoreStream item ->
                          stream monad (Either SomeException (Persisted item))
streamAllInfinitely eventStoreStream =
  (EventStore.Subscribing.subscribe eventStoreStream)
    `parallel` (streamAll eventStoreStream)


recordedEventToPersistedItem :: FromJSON item => EventStore.RecordedEvent -> Persisted item
recordedEventToPersistedItem recordedEvent =
  PersistedItem { offset = toInteger $ EventStore.recordedEventNumber recordedEvent,
                  item = fromJust $ EventStore.recordedEventDataAsJson recordedEvent }


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