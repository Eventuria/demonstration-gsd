{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Gsd.Write.Command.Arbitrary where

import Test.QuickCheck
import Generic.Random
import Test.QuickCheck.Instances.UnorderedContainers ()
import Test.QuickCheck.Instances.Vector ()
import Test.QuickCheck.Instances.Scientific ()
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Time ()
import Gsd.Write.Model.Commands.Command
import Test.QuickCheck.Instances.UUID ()

instance Arbitrary GsdCommand where
  arbitrary :: Gen  GsdCommand
  arbitrary = genericArbitraryU
