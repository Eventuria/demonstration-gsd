{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Eventuria.Libraries.CQRS.Write.PersistCommandResult where

import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import Data.Aeson
import qualified Data.Text as Text
import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId

data PersistCommandResult = FailedToPersist {aggregateId :: AggregateId ,
                                             commandId :: CommandId ,
                                             reason :: String}
                          | SuccessfullyPersisted { aggregateId :: AggregateId ,
                                                    commandId :: CommandId ,
                                                    lastOffsetPersisted :: Offset} deriving Show



successfullyPersistedName :: String
successfullyPersistedName = "success"

failedToPersistName :: String
failedToPersistName = "failure"

instance ToJSON PersistCommandResult where
   toJSON (SuccessfullyPersisted {aggregateId,commandId,lastOffsetPersisted}) = object [
             "persistCommandResult" .= successfullyPersistedName,
             "aggregateId" .= aggregateId,
             "commandId" .= commandId,
             "lastOffsetPersisted" .= lastOffsetPersisted]
   toJSON (FailedToPersist {aggregateId,commandId, reason}) = object [
             "persistCommandResult" .= failedToPersistName,
             "aggregateId" .= aggregateId,
             "commandId" .= commandId,
             "reason" .= reason]

instance FromJSON PersistCommandResult  where
    parseJSON (Object jsonObject) =  do
                   persistenceResult <- jsonObject .: "persistCommandResult"
                   case persistenceResult of
                        Just (String persistenceResultName)
                          | (Text.unpack persistenceResultName) == successfullyPersistedName ->
                              SuccessfullyPersisted
                                  <$> jsonObject .: "aggregateId"
                                  <*> jsonObject .: "commandId"
                                  <*> jsonObject .: "lastOffsetPersisted"
                        Just (String persistenceResultName)
                          | (Text.unpack persistenceResultName) == failedToPersistName ->
                              FailedToPersist
                                  <$> jsonObject .: "aggregateId"
                                  <*> jsonObject .: "commandId"
                                  <*> jsonObject .:  "reason"
                        Just (String unknownResponseName) ->
                          error $ "FromJSON PersistCommandResult response unknown : " ++ Text.unpack unknownResponseName
                        Nothing -> error $ "FromJSON PersistCommandResult status not provided"
                        _ -> error $ "FromJSON PersistCommandResult Json format not expected"

    parseJSON _ = error $ "FromJSON PersistCommandResult Json format not expected"
