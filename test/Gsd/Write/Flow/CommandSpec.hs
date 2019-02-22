{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Gsd.Write.Flow.CommandSpec (main, spec)  where

import Test.Hspec
import Test.QuickCheck
import Gsd.Write.Model.Commands.Serialization ()
import Gsd.Write.Model.Commands.Command
import Gsd.Write.Flow.Arbitrary ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Gsd Command" $ do
    it "can converted back and forth to CQRS Command"
      $  verbose
      $ \gsdCommand -> ((fromCommand . toCommand) gsdCommand) == (gsdCommand :: GsdCommand)

