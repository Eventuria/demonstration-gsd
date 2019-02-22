{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Eventuria.Libraries.CQRS.Write.Serialization.ValidationStateSpec (main, spec)  where

import Test.Hspec
import Test.QuickCheck
import Generic.Random
import Data.Aeson
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.ValidationStates.ValidationState
import Eventuria.Libraries.CQRS.Write.Serialization.ValidationState ()
import Test.QuickCheck.Instances.UUID ()
import Test.QuickCheck.Instances.UnorderedContainers ()
import Test.QuickCheck.Instances.Vector ()
import Test.QuickCheck.Instances.Scientific ()
import Test.QuickCheck.Instances.Text ()


instance Arbitrary a => Arbitrary (ValidationState a) where
  arbitrary :: Gen (ValidationState a)
  arbitrary = genericArbitraryU

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Validation State" $ do
    it "can be marshalled and unmarshalled"
      $ verbose
      $ \validationState -> ((decode . encode) validationState) == (Just (validationState) :: Maybe (ValidationState Bool))