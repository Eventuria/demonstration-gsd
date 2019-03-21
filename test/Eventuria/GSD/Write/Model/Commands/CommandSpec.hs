{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Eventuria.GSD.Write.Model.Commands.CommandSpec (main, spec)  where

import Test.Hspec
import Test.QuickCheck

import Data.Aeson

import Eventuria.GSD.Write.Model.Commands.Serialization ()
import Eventuria.GSD.Write.Model.Commands.Command

import Eventuria.GSD.Write.Model.Commands.PropertyTesting.Command ()
import Eventuria.GSD.Write.Model.Commands.Serialization ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Gsd Command" $ do
    it "can be converted back and forth to CQRS Command"
      $ property
      $ \gsdCommand -> ((fromCommand . toCommand) gsdCommand) == (gsdCommand :: GsdCommand)


    it "can be marshalled and unmarshalled"
      $ property
      $ \command -> ((decode . encode) command) == (Just (command) :: Maybe (GsdCommand))


