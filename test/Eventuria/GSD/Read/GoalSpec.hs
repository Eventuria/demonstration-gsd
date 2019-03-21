{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Eventuria.GSD.Read.GoalSpec (main, spec)  where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.UUID ()
import Test.QuickCheck.Instances.UnorderedContainers ()
import Test.QuickCheck.Instances.Vector ()
import Test.QuickCheck.Instances.Scientific ()
import Test.QuickCheck.Instances.Text ()

import Generic.Random

import Data.Aeson

import Eventuria.GSD.Read.Model.Goal
import Eventuria.GSD.Read.Model.ActionStats

instance Arbitrary ActionStats where
  arbitrary :: Gen  ActionStats
  arbitrary = genericArbitraryU

instance Arbitrary GoalStatus where
  arbitrary :: Gen  GoalStatus
  arbitrary = genericArbitraryU

instance Arbitrary Goal where
  arbitrary :: Gen  Goal
  arbitrary = genericArbitraryU


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Goal" $ do
    it "can be marshalled and unmarshalled"
      $ property
      $ \goal -> ((decode . encode) goal) == (Just (goal) :: Maybe (Goal))
