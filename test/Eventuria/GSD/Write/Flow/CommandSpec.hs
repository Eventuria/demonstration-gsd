{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Eventuria.GSD.Write.CommandSpec (main, spec)  where

import Test.Hspec
import Test.QuickCheck
import Eventuria.GSD.Write.Model.Commands.Serialization ()
import Eventuria.GSD.Write.Model.Commands.Command
import Eventuria.GSD.Write.Arbitrary ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Gsd Command" $ do
    it "can converted back and forth to CQRS Command"
      $  verbose
      $ \gsdCommand -> ((fromCommand . toCommand) gsdCommand) == (gsdCommand :: GsdCommand)

