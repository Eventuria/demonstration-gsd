{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Commands.CommandsSpec (main, spec)  where

import Data.Aeson.QQ
import Data.Aeson (Value)
import Test.Hspec
import Test.QuickCheck
import Generic.Random
import Data.Aeson
import Cqrs.Commands.Command
import Cqrs.Commands.Serialization
import Test.QuickCheck.Instances.UUID


instance Arbitrary Command where
  arbitrary :: Gen Command
  arbitrary = genericArbitraryU

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Commands" $ do
    it "can be marshalled and unmarshalled"
      $  verbose
      $ \command -> (decode $ encode command) == (Just (command) :: Maybe Command)