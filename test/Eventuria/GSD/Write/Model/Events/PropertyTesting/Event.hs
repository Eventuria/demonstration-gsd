{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Eventuria.GSD.Write.Model.Events.PropertyTesting.Event where

import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Time ()
import Test.QuickCheck.Instances.UUID ()

import Generic.Random

import Eventuria.GSD.Write.Model.Events.Event

instance Arbitrary GsdEvent where
  arbitrary :: Gen  GsdEvent
  arbitrary = genericArbitraryU
