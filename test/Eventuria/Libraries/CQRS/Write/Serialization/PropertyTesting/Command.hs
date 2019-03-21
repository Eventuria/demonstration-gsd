{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Eventuria.Libraries.CQRS.Write.Serialization.PropertyTesting.Command where

import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Time ()
import Test.QuickCheck.Instances.UUID ()

import Generic.Random

import Eventuria.Adapters.Aeson.PropertyTesting.Value ()

import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.CommandHeader
import Eventuria.Libraries.CQRS.Write.Aggregate.Commands.Command

instance  Arbitrary (CommandHeader) where
  arbitrary :: Gen CommandHeader
  arbitrary = genericArbitraryU


instance  Arbitrary (Command) where
  arbitrary :: Gen Command
  arbitrary = genericArbitraryU