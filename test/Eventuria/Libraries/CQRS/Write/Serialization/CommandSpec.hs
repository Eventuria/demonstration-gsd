{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Eventuria.Libraries.CQRS.Write.Serialization.CommandSpec (main, spec)  where

import Test.Hspec

import Test.QuickCheck

import Data.Aeson

import Eventuria.Libraries.CQRS.Write.Serialization.PropertyTesting.Command ()

import Eventuria.Libraries.CQRS.Write.Serialization.CommandHeader ()
import Eventuria.Libraries.CQRS.Write.Serialization.Command ()
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "CQRS Commands" $ do
    it "can be marshalled and unmarshalled"
      $ property
      $ \command -> ((decode . encode) command) == (Just (command) :: Maybe Command)