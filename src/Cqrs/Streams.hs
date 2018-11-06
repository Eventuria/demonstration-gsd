{-# LANGUAGE OverloadedStrings #-}
module Cqrs.Streams where
import Data.Aeson
import qualified Data.Text as Text


type Offset = Integer

data PersistenceFailure = ItemAlreadyPersisted
data PersistResult = PersistResult {writeNextVersion :: Integer}

instance ToJSON PersistResult where
   toJSON (PersistResult writeNextVersion) = object [
             "writeNextVersion" .= writeNextVersion]


instance FromJSON PersistResult  where

    parseJSON (Object jsonObject) = PersistResult <$> jsonObject .: "writeNextVersion"


instance ToJSON PersistenceFailure where
   toJSON (ItemAlreadyPersisted) = object [("errorName" ,"ItemAlreadyPersisted")]


instance FromJSON PersistenceFailure  where

    parseJSON (Object jsonObject) = do
             errorNameMaybe <- jsonObject .: "errorName"
             case errorNameMaybe of
                  Just (String errorName) | (Text.unpack errorName) == "ItemAlreadyPersisted" -> return ItemAlreadyPersisted
                  Nothing -> error $ "error name not provided or invalid"
