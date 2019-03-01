{-# LANGUAGE OverloadedStrings #-}
module Eventuria.Libraries.CQRS.Write.Serialization.PersistenceResult where

import Data.Aeson
import Eventuria.Libraries.PersistedStreamEngine.Interface.Write.PersistenceResult

persistenceSuccessName :: String
persistenceSuccessName = "success"

persistenceFailureName :: String
persistenceFailureName = "failure"

instance ToJSON PersistenceResult where
   toJSON (PersistenceResult lastOffsetPersisted) = object ["lastOffsetPersisted" .= lastOffsetPersisted]

instance FromJSON PersistenceResult  where
    parseJSON (Object jsonObject) =  PersistenceResult <$> jsonObject .: "lastOffsetPersisted"
    parseJSON _ = error $ "Json format not expected"
