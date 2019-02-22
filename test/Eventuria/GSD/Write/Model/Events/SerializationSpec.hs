{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Eventuria.GSD.Write.Events.SerializationSpec (main, spec)  where

import Test.Hspec
import Test.QuickCheck
import Generic.Random
import Data.Aeson

import Test.QuickCheck.Instances.UUID ()

import Test.QuickCheck.Instances.UnorderedContainers ()
import Test.QuickCheck.Instances.Vector ()
import Test.QuickCheck.Instances.Scientific ()
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Time ()
import Eventuria.GSD.Write.Model.Events.Serialization ()
import Eventuria.GSD.Write.Model.Events.Event


instance Arbitrary GsdEvent where
  arbitrary :: Gen  GsdEvent
  arbitrary = genericArbitraryU

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Gsd Events" $ do
    it "can be marshalled and unmarshalled"
      $  verbose
      $ \event -> ((decode . encode) event) == (Just (event) :: Maybe (GsdEvent))
