{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module System.SafeResponse where

import Control.Exception
import Data.Aeson
import Data.Text


type SafeResponse result = Either SomeException result


-- Temporary ...

instance ToJSON SomeException  where
  toJSON (exception) = String $ (pack . show) exception

-- Temporary ...

data ClientExceptionBadlyDeserialized = ClientExceptionBadlyDeserialized String  deriving Show

instance Exception ClientExceptionBadlyDeserialized

instance FromJSON SomeException   where

    parseJSON (String text) = return $ toException $ ClientExceptionBadlyDeserialized (unpack text)
    parseJSON _ =  error $ "FromJSON SomeException : Json format not expected"



