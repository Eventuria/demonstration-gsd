{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Eventuria.Libraries.CQRS.Write.Serialization.CommandTransactionSpec (main, spec)  where

import Test.Hspec
import Test.QuickCheck

import Generic.Random

import Data.Aeson

import Eventuria.Libraries.CQRS.Write.Serialization.PropertyTesting.CommandTransaction ()
import Eventuria.Libraries.CQRS.Write.Serialization.CommandTransaction ()
import Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandTransaction
import Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandHandlingResult
import Eventuria.Libraries.CQRS.Write.Aggregate.Events.Event



main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Command Transaction" $ do
    it "can be marshalled and unmarshalled"
      $ property
      $ \commandTransaction -> ((decode . encode) commandTransaction) == (Just (commandTransaction) :: Maybe (CommandTransaction))