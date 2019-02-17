{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Gsd.Read.ActionSpec (main, spec)  where

import Test.Hspec
import Test.QuickCheck
import Generic.Random
import Data.Aeson
import Gsd.Read.Model.Action
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



main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Action" $ do
    it "can be marshalled and unmarshalled"
      $  verbose
      $ \action -> ((decode . encode) action) == (Just (action) :: Maybe (Action))
