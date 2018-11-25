{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Cqrs.Aggregate.Commands.Responses.CommandResponseStream (
readForward,
persist) where

import Streamly
import qualified Streamly.Prelude as S
import Control.Monad.IO.Class (MonadIO(..))



import qualified Database.EventStore as EventStore
import qualified Cqrs.EventStore.Writing as EventStore.Writing
import Cqrs.Aggregate.Ids.AggregateId
import Cqrs.Aggregate.Commands.Responses.CommandResponse

import Control.Concurrent.Async (wait)
import qualified Data.Text as Text
import Data.UUID
import Cqrs.EventStore.Context
import Data.Maybe

import Cqrs.Streams


persist :: EventStoreContext -> CommandResponse -> IO (Either PersistenceFailure PersistResult)
persist context commandResponse = EventStore.Writing.persist context (getStreamName $ getAggregateId commandResponse) commandResponse


readForward :: (IsStream stream, MonadIO (stream IO), Semigroup (stream IO CommandResponse)) => EventStore.Credentials -> EventStore.Connection -> AggregateId -> Offset -> stream IO CommandResponse
readForward credentials eventStoreConnection  workspaceId fromOffset =  do
               let batchSize = 100 :: Integer
                   resolveLinkTos = False
               asyncRead <- liftIO $ EventStore.readStreamEventsForward
                                eventStoreConnection
                                (getStreamName workspaceId)
                                (fromInteger fromOffset)
                                (fromInteger batchSize)
                                resolveLinkTos
                                (Just credentials)
               commandFetched <- liftIO $ wait asyncRead
               case commandFetched of
                    EventStore.ReadSuccess readResult -> do
                        let commandResponses = getCommandResponseRequestFromReadResult readResult
                        if (length commandResponses) /= 0 then do
                            (S.fromList commandResponses) <> (readForward credentials eventStoreConnection workspaceId $ fromOffset + batchSize)
                        else S.fromList commandResponses
                    e -> error $ "Read failure: " <> show e


getCommandResponseRequestFromReadResult :: EventStore.StreamSlice -> [CommandResponse]
getCommandResponseRequestFromReadResult sl = catMaybes $ EventStore.resolvedEventDataAsJson <$> EventStore.sliceEvents sl

getStreamName :: AggregateId -> EventStore.StreamName
getStreamName workspaceId = EventStore.StreamName $ Text.pack $ "aggregate_response_command-" ++ toString workspaceId

instance EventStore.Writing.Persistable CommandResponse where
  getItemName commandResponse  = getCommandResponseName commandResponse