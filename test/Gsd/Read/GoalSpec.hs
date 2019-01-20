{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Gsd.Read.GoalSpec (main, spec)  where

import Test.Hspec
import Test.QuickCheck
import Generic.Random
import Data.Aeson
import Gsd.Read.Goal
import Test.QuickCheck.Instances.UUID ()
import Test.QuickCheck.Instances.UnorderedContainers ()
import Test.QuickCheck.Instances.Vector ()
import Test.QuickCheck.Instances.Scientific ()
import Test.QuickCheck.Instances.Text ()


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
  describe "Action" $ do
    it "can be marshalled and unmarshalled"
      $  verbose
      $ \goal -> ((decode . encode) goal) == (Just (goal) :: Maybe (Goal))
