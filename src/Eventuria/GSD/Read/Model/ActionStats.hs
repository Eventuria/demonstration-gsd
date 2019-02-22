{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module Eventuria.GSD.Read.Model.ActionStats where

import Data.Aeson
import GHC.Generics

data ActionStats = ActionStats {
                    total :: Integer,
                    completed :: Integer,
                    opened :: Integer } deriving (Show , Eq , Generic )

instance ToJSON ActionStats where
  toJSON (ActionStats {total, completed ,opened } ) = object [
            "total" .= total,
            "completed" .= completed,
            "opened" .= opened]

instance FromJSON ActionStats  where

    parseJSON (Object jsonObject) = ActionStats
                                      <$> jsonObject .: "total"
                                      <*>  jsonObject .: "completed"
                                      <*>  jsonObject .: "opened"
    parseJSON _ =  error $ "Json format not expected"