{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Eventuria.GSD.Write.Model.Commands.PropertyTesting.Command where

import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Time ()
import Test.QuickCheck.Instances.UUID ()

import Generic.Random

import Eventuria.GSD.Write.Model.Commands.Command

instance Arbitrary GSDCommand where
  arbitrary :: Gen  GSDCommand
  arbitrary = genericArbitraryU

