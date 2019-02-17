{-# LANGUAGE OverloadedStrings #-}
module CQRS.Write.Serialization.PersistenceResult where

import Data.Aeson
import qualified Data.Text as Text
import PersistedStreamEngine.Interface.Write.PersistenceResult

persistenceSuccessName :: String
persistenceSuccessName = "success"

persistenceFailureName :: String
persistenceFailureName = "failure"

instance ToJSON PersistenceResult where
   toJSON (PersistenceSuccess lastOffsetPersisted) = object [
             "persistenceResult" .= persistenceSuccessName,
             "lastOffsetPersisted" .= lastOffsetPersisted]
   toJSON (PersistenceFailure reason) = object [
             "persistenceResult" .= persistenceFailureName,
             "reason" .= reason]

instance FromJSON PersistenceResult  where
    parseJSON (Object jsonObject) =  do
                   persistenceResult <- jsonObject .: "persistenceResult"
                   case persistenceResult of
                        Just (String persistenceResultName)
                          | (Text.unpack persistenceResultName) == persistenceSuccessName ->
                              PersistenceSuccess
                                <$> jsonObject .: "lastOffsetPersisted"
                        Just (String persistenceResultName)
                          | (Text.unpack persistenceResultName) == persistenceFailureName ->
                              PersistenceFailure
                                <$> jsonObject .:  "reason"
                        Just (String unknownResponseName) ->
                           error $ "Persistence response unknown : " ++ Text.unpack unknownResponseName
                        Nothing -> error $ "persistenceResult status not provided"
                        _ -> error $ "Json format not expected"

    parseJSON _ = error $ "Json format not expected"
