{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Eventuria.Libraries.CQRS.Write.PersistCommandResult where

import Data.Aeson

import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandId
import Eventuria.Libraries.PersistedStreamEngine.Interface.Offset
import Eventuria.Libraries.CQRS.Write.Aggregate.Ids.AggregateId

data PersistCommandResult =  PersistCommandResult { aggregateId :: AggregateId ,
                                                    commandId :: CommandId ,
                                                    lastOffsetPersisted :: Offset} deriving Show



successfullyPersistedName :: String
successfullyPersistedName = "success"

failedToPersistName :: String
failedToPersistName = "failure"

instance ToJSON PersistCommandResult where
   toJSON (PersistCommandResult {aggregateId,commandId,lastOffsetPersisted}) = object [
             "aggregateId" .= aggregateId,
             "commandId" .= commandId,
             "lastOffsetPersisted" .= lastOffsetPersisted]


instance FromJSON PersistCommandResult  where
    parseJSON (Object jsonObject) =  PersistCommandResult
                                           <$> jsonObject .: "aggregateId"
                                           <*> jsonObject .: "commandId"
                                           <*> jsonObject .: "lastOffsetPersisted"

    parseJSON _ = error $ "FromJSON PersistCommandResult Json format not expected"
