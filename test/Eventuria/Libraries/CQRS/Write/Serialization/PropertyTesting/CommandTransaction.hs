{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Eventuria.Libraries.CQRS.Write.Serialization.PropertyTesting.CommandTransaction where

import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Time ()
import Test.QuickCheck.Instances.UUID ()

import Generic.Random

import Eventuria.Adapters.Aeson.PropertyTesting.Value ()

import Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandTransaction
import Eventuria.Libraries.CQRS.Write.CommandConsumption.CommandHandlingResult ()
import Eventuria.Libraries.CQRS.Write.Aggregate.Events.Event ()

instance Arbitrary EventHeader where
  arbitrary :: Gen EventHeader
  arbitrary = genericArbitraryU

instance Arbitrary Event where
  arbitrary :: Gen Event
  arbitrary = genericArbitraryU

instance Arbitrary CommandHandlingResult where
  arbitrary :: Gen CommandHandlingResult
  arbitrary = genericArbitraryU

instance Arbitrary CommandTransaction where
  arbitrary :: Gen CommandTransaction
  arbitrary = genericArbitraryU