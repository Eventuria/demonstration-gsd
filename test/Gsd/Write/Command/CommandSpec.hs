{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Gsd.Write.Command.CommandSpec (main, spec)  where

import Test.Hspec
import Test.QuickCheck
import Gsd.Write.Model.Commands.Serialization ()
import Gsd.Write.Model.Commands.Command
import Gsd.Write.Command.Arbitrary ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Gsd Command" $ do
    it "can converted back and forth to Cqrs Command"
      $  verbose
      $ \gsdCommand -> ((fromCommand . toCommand) gsdCommand) == (gsdCommand :: GsdCommand)

