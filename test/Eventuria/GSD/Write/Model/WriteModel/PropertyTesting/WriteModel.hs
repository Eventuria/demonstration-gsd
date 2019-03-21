{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Eventuria.GSD.Write.Model.WriteModel.PropertyTesting.WriteModel where

import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Time ()
import Test.QuickCheck.Instances.UUID ()

import Generic.Random

import Eventuria.GSD.Write.Model.WriteModel

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