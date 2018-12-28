{-# LANGUAGE OverloadedStrings #-}
module PersistedStreamEngine.Interface.PersistedItem where

import Data.Aeson
import PersistedStreamEngine.Interface.Offset

data Persisted item = PersistedItem {
                                offset :: Offset ,
                                item :: item} deriving Show

instance (ToJSON item) => ToJSON (Persisted item) where
  toJSON (PersistedItem{ offset = offset , item = item} ) = object [
            "offset" .= offset,
            "item" .= item]

instance  (FromJSON item) => FromJSON (Persisted item)  where

    parseJSON (Object jsonObject) = PersistedItem <$> jsonObject .: "offset" <*>  jsonObject .: "item"
    parseJSON _ =  error $ "Json format not expected"