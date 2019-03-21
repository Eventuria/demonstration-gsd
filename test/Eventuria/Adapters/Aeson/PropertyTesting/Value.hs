{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Eventuria.Adapters.Aeson.PropertyTesting.Value where

import Test.QuickCheck
import Test.QuickCheck.Instances.UnorderedContainers ()
import Test.QuickCheck.Instances.Vector ()
import Test.QuickCheck.Instances.Scientific ()
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Time ()
import Test.QuickCheck.Instances.UUID ()

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.HashMap.Lazy as HashMap

import Generic.Random


instance Arbitrary Aeson.Value where
  arbitrary = sized sizedArbitraryValue

sizedArbitraryValue :: Int -> Gen Aeson.Value
sizedArbitraryValue n
  | n <= 0 = oneof [pure Aeson.Null, bool, number, string]
  | otherwise = resize n' $ oneof [pure Aeson.Null, bool, number, string, array, object']
  where
    n' = n `div` 2
    bool = Aeson.Bool <$> arbitrary
    number = (Aeson.Number . fromRational . toRational :: Double -> Aeson.Value) <$> arbitrary
    string = (Aeson.String . Text.pack) <$> arbitrary
    array = (Aeson.Array . Vector.fromList) <$> arbitrary
    object' = (Aeson.Object . HashMap.fromList) <$> arbitrary