{-# LANGUAGE InstanceSigs, TypeApplications #-}
module Eventuria.GSD.Write.SerializationSpec (main, spec)  where


import Test.Hspec
import Test.QuickCheck
import Eventuria.GSD.Write.Model.Commands.Serialization ()
import Eventuria.GSD.Write.Model.Commands.Command
import Data.Aeson
import Eventuria.GSD.Write.Arbitrary ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Gsd Command" $ do
    it "can be marshalled and unmarshalled"
      $  verbose
      $ \command -> ((decode . encode) command) == (Just (command) :: Maybe (GsdCommand))

