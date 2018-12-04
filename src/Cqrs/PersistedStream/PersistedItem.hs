{-# LANGUAGE OverloadedStrings #-}
module Cqrs.PersistedStream.PersistedItem where

import Data.Aeson
import Cqrs.Streams

data Persisted item = PersistedItem {
                                offset :: Offset ,
                                item :: item} deriving Show

instance (ToJSON item) => ToJSON (Persisted item) where
  toJSON (PersistedItem{ offset = offset , item = item} ) = object [
            "offset" .= offset,
            "item" .= item]
