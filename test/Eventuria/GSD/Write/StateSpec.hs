{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Eventuria.GSD.Write.StateSpec (main, spec)  where

import Test.Hspec
import Test.QuickCheck
import Generic.Random
import Data.Aeson
import Eventuria.GSD.Write.Model.WriteModel
import Test.QuickCheck.Instances.UUID ()
import Test.QuickCheck.Instances.UnorderedContainers ()
import Test.QuickCheck.Instances.Vector ()
import Test.QuickCheck.Instances.Scientific ()
import Test.QuickCheck.Instances.Text ()


instance Arbitrary ActionStatus where
  arbitrary :: Gen  ActionStatus
  arbitrary = genericArbitraryU

instance Arbitrary Action where
  arbitrary :: Gen  Action
  arbitrary = genericArbitraryU

instance Arbitrary GoalStatus where
  arbitrary :: Gen  GoalStatus
  arbitrary = genericArbitraryU


instance Arbitrary Goal where
  arbitrary :: Gen  Goal
  arbitrary = genericArbitraryU

instance Arbitrary GsdWriteModel where
  arbitrary :: Gen  GsdWriteModel
  arbitrary = genericArbitraryU

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Gsd State" $ do
    it "can be marshalled and unmarshalled"
      $  verbose
      $ \gsdState -> ((decode . encode) gsdState) == (Just (gsdState) :: Maybe (GsdWriteModel))
