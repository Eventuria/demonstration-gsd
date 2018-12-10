{-# LANGUAGE OverloadedStrings #-}
module PersistedStreamEngine.PersistedItem where

import Data.Aeson
import PersistedStreamEngine.Offset

data Persisted item = PersistedItem {
                                offset :: Offset ,
                                item :: item} deriving Show

instance (ToJSON item) => ToJSON (Persisted item) where
  toJSON (PersistedItem{ offset = offset , item = item} ) = object [
            "offset" .= offset,
            "item" .= item]
