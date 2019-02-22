{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Eventuria.GSD.Read.Model.GoalStats where

import Data.Aeson

data GoalStats = GoalStats {total :: Integer, accomplished :: Integer,  toBeAccomplished :: Integer } deriving Show

instance ToJSON GoalStats where
  toJSON (GoalStats {total, accomplished ,toBeAccomplished } ) = object [
            "total" .= total,
            "accomplished" .= accomplished,
            "toBeAccomplished" .= toBeAccomplished]

instance FromJSON GoalStats  where

    parseJSON (Object jsonObject) = GoalStats <$> jsonObject .: "total" <*>  jsonObject .: "accomplished" <*>  jsonObject .: "toBeAccomplished"
    parseJSON _ =  error $ "Json format not expected"

