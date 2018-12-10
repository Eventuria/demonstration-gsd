{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module PersistedStreamEngine.Instances.EventStore.Read.Streaming where

import Streamly
import qualified Streamly.Prelude as S
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent.Async (wait)

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



streamAllInfinitely :: Streamable stream monad item => EventStoreStream item -> stream monad (Persisted item)
streamAllInfinitely eventStoreStream =
  (EventStore.Subscribing.subscribe eventStoreStream)
    `parallel` (streamAll eventStoreStream)

streamAll :: Streamable stream monad item =>
                EventStoreStream item ->
                stream monad (Persisted item)
streamAll eventStoreStream = streamFromOffset eventStoreStream 0

streamFromOffset :: Streamable stream monad item =>
                      EventStoreStream item ->
                      Offset ->
                      stream monad (Persisted item)




streamFromOffset eventStoreStream @ EventStoreStream {
                                       settings = EventStoreSettings { logger, credentials, connection },
                                       streamName = streamName } fromOffset = do
     liftIO $ logInfo logger $ "streaming [" ++ (show fromOffset) ++ "..] > " ++ show streamName

     let batchSize = 100
         resolveLinkTos = False
     asyncRead <- liftIO $ EventStore.readStreamEventsForward
                      connection
                      streamName
                      (fromInteger fromOffset)
                      (fromInteger batchSize)
                      resolveLinkTos
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